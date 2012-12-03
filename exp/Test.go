package main

import (
  "fmt"
  "os"
  fp "path/filepath"
  "net/http"
  "io/ioutil"
)

func TestPath() {
  const pat = "?ur*.hs"
  visit := func (path string, fi os.FileInfo, err error) error {
    if m, _ := fp.Match(pat, path); m {
      fmt.Printf("%s\n", path)
    }
    return nil
  }

  fp.Walk(".", visit)
}

func TestUrlGet() {
  r, e := http.Get("http://google.com")
  if e != nil {
    fmt.Printf("ERROR: %v\n", e)
    return
  }
  c, e := ioutil.ReadAll(r.Body)
  r.Body.Close()
  if e != nil {
    fmt.Printf("ERROR: %v\n", e)
    return
  }
  fmt.Printf("%v\n", string(c))
}

func main() {
  TestUrlGet()
}
