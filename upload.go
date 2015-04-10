package main

import (
	"crypto/md5"
	"flag"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/awslabs/aws-sdk-go/aws"
	"github.com/awslabs/aws-sdk-go/service/s3"
)

const (
	sniffLen = 512
	bucket   = "www.deepak.jois.name"
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
func computeMd5(path string) (string, error) {
	var result []byte
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hash := md5.New()
	if _, err := io.Copy(hash, file); err != nil {
		return "", err
	}

	s := fmt.Sprintf("%x", hash.Sum(result))
	return s, nil
}

// Calculate MD5 for file in S3 using ETag header
func computeMd5Remote(path string) (string, error) {
	key := strings.TrimPrefix(path, pathPrefix)
	key = strings.TrimPrefix(key, "/") // Just in case

	params := &s3.HeadObjectInput{
		Bucket: aws.String(bucket), // Required
		Key:    aws.String(key),    // Required
	}

	resp, err := svc.HeadObject(params)

	if awserr := aws.Error(err); awserr != nil {
		return "", awserr
	} else if err != nil {
		return "", err
	}

	return strings.Trim(*resp.ETag, "\""), nil
}

// Upload file to S3
func uploadFile(path string, ctype string) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}

	key := strings.TrimPrefix(path, pathPrefix)
	key = strings.TrimPrefix(key, "/") // Just in case

	params := &s3.PutObjectInput{
		Bucket:      aws.String(bucket), // Required
		Key:         aws.String(key),    // Required
		Body:        file,
		ContentType: aws.String(ctype),
	}

	_, err = svc.PutObject(params)

	if awserr := aws.Error(err); awserr != nil {
		return awserr
	} else if err != nil {
		return err
	}
	return nil
}

func visit(path string, f os.FileInfo, err error) error {
	if f.IsDir() {
		return nil
	}

	fmt.Printf("Uploading %s…", path)

	md5Local, err := computeMd5(path)
	if err != nil {
		return err
	}

	md5Remote, err := computeMd5Remote(path)
	if awserr := aws.Error(err); awserr != nil {
		if awserr.StatusCode == 404 {
			md5Remote = "" // Set to dummy value
		} else {
			return err
		}

	} else if err != nil {
		return err
	}

	if md5Local == md5Remote {
		fmt.Println("up-to-date")
	} else {
		ctype, err := sniffContentType(path)
		if err != nil {
			return err
		}

		err = uploadFile(path, ctype)
		if err != nil {
			fmt.Println("ERROR!")
		} else {
			fmt.Println("done")
		}
	}
	return nil
}

func main() {
	svc = s3.New(&aws.Config{Region: "us-east-1"})
	flag.Parse()
	pathPrefix = flag.Arg(0)
	err := filepath.Walk(pathPrefix, visit)
	if err != nil {
		fmt.Printf("filepath.Walk() returned %v\n", err)
	}
}
