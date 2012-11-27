package main

import ("fmt"; "os"; "path/filepath")

func main() {
  const pat = "?ur*.hs"
  visit := func (path string, fi os.FileInfo, err error) error {
    if m, _ := filepath.Match(pat, path); m {
      fmt.Printf("%s\n", path)
    }
    return nil
  }

  filepath.Walk(".", visit)
}
