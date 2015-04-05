package main

import (
	"flag"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"path/filepath"
)

const sniffLen = 512

func visit(path string, f os.FileInfo, err error) error {
	if f.IsDir() {
		return nil
	}
	ctype := mime.TypeByExtension(filepath.Ext(f.Name()))
	if ctype == "" {
		// read a chunk to decide between utf-8 text and binary
		var buf [sniffLen]byte
		content, err := os.Open(path) // For read access.
		if err != nil {
			return err
		}
		n, _ := io.ReadFull(content, buf[:])
		ctype = http.DetectContentType(buf[:n])
	}
	fmt.Printf("Upload %s with Content-type %s \n", path, ctype)
	return nil
}

func main() {
	flag.Parse()
	err := filepath.Walk(flag.Arg(0), visit)
	if err != nil {
		fmt.Printf("filepath.Walk() returned %v\n", err)
	}

}
