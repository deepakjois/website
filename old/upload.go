package main

import (
	"bytes"
	"compress/gzip"
	"crypto/md5"
	"errors"
	"flag"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
)

const (
	sniffLen    = 512
	bucket      = "www.deepak.jois.name"
	numParallel = 10
)

var (
	svc        *s3.S3
	pathPrefix string
)

// Sniff ContentType to use using file extension if possible. If not, do
// sniffing.
func sniffContentType(path string) (string, error) {
	ctype := mime.TypeByExtension(filepath.Ext(path))
	if ctype == "" {
		// read a chunk to decide between utf-8 text and binary
		var buf [sniffLen]byte
		content, err := os.Open(path) // For read access.
		if err != nil {
			return "", err
		}
		n, _ := io.ReadFull(content, buf[:])
		ctype = http.DetectContentType(buf[:n])
	}
	return ctype, nil
}

// Calculate MD5 of local file
func computeMd5(buf io.Reader) (string, error) {
	var result []byte
	hash := md5.New()
	if _, err := io.Copy(hash, buf); err != nil {
		return "", err
	}

	s := fmt.Sprintf("%x", hash.Sum(result))
	return s, nil
}

// Calculate MD5 for file in S3 using ETag header
func computeMd5Remote(key string) (string, error) {
	params := &s3.HeadObjectInput{
		Bucket: aws.String(bucket), // Required
		Key:    aws.String(key),    // Required
	}

	resp, err := svc.HeadObject(params)

	if err != nil {
		return "", err
	}

	return strings.Trim(*resp.ETag, "\""), nil
}

// Upload file to S3
func uploadFile(r io.ReadSeeker, key, ctype string) error {

	params := &s3.PutObjectInput{
		Bucket:          aws.String(bucket), // Required
		Key:             aws.String(key),    // Required
		Body:            r,
		ContentType:     aws.String(ctype),
		ContentEncoding: aws.String("gzip"),
	}

	if _, err := svc.PutObject(params); err != nil {
		return err
	}

	return nil
}

// Check if MD5 hashes of remote and (gzipped) local copies match, and upload
// local to remote if they don’t.
func checkAndUpload(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}

	var b bytes.Buffer
	gw := gzip.NewWriter(&b)
	_, err = io.Copy(gw, file)
	gw.Close()
	file.Close()
	if err != nil {
		return "", err
	}

	md5Local, err := computeMd5(bytes.NewReader(b.Bytes()))
	if err != nil {
		return "", err
	}

	key := strings.TrimPrefix(path, pathPrefix)
	key = strings.TrimPrefix(key, "/") // Just in case

	md5Remote, err := computeMd5Remote(key)
	if reqErr, ok := err.(awserr.RequestFailure); ok {
		if reqErr.StatusCode() == 404 {
			md5Remote = "" // Set to dummy value
		} else {
			return "", err
		}
	} else if err != nil {
		return "", err
	}

	if md5Local == md5Remote {
		return "up-to-date", nil
	}

	ctype, err := sniffContentType(path)
	if err != nil {
		return "", err
	}

	err = uploadFile(bytes.NewReader(b.Bytes()), key, ctype)
	if err != nil {
		return "", err
	}
	return "done", nil
}

type result struct {
	path   string
	status string
	err    error
}

func uploader(done <-chan struct{}, paths <-chan string, c chan<- result) {
	for path := range paths {
		status, err := checkAndUpload(path)
		select {
		case c <- result{path: path, status: status, err: err}:
		case <-done:
			return
		}
	}
}

func main() {
	svc = s3.New(session.New(aws.NewConfig().WithRegion("us-east-1")))

	flag.Parse()
	pathPrefix = flag.Arg(0)

	done := make(chan struct{})
	defer close(done)

	paths := make(chan string)
	errc := make(chan error, 1)

	go func() {
		defer close(paths)

		errc <- filepath.Walk(pathPrefix, func(path string, f os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if f.IsDir() {
				if filepath.Base(f.Name()) == "Drafts" {
					return filepath.SkipDir
				}
				return nil
			}
			select {
			case paths <- path:
			case <-done:
				return errors.New("walk canceled")
			}
			return nil
		})
	}()

	// Start a fixed number of goroutines to check and upload files.
	c := make(chan result)
	var wg sync.WaitGroup
	wg.Add(numParallel)
	for i := 0; i < numParallel; i++ {
		go func() {
			uploader(done, paths, c)
			wg.Done()
		}()
	}
	go func() {
		wg.Wait()
		close(c)
	}()

	for r := range c {
		if r.status == "done" {
			fmt.Printf("Uploading %s…%s\n", r.path, r.status)
		}
		if r.err != nil {
			fmt.Printf("Uploading %s…ERROR!: %s\n", r.path, r.err)
		}
	}

	// Check whether the Walk failed.
	if err := <-errc; err != nil {
		fmt.Print("Walk failed")
	}
}
