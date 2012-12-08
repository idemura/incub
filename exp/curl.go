package main

import (
  "fmt"
  "os"
  "net/http"
  "io/ioutil"
)

const usage_str = 
`Usage: curl <url>
Downloads file from <url> and outputs to stdout
`

func main() {
  args := os.Args[1:]
  if len(args) != 1 {
    fmt.Fprintf(os.Stderr, usage_str)
    os.Exit(-1)
  }

  r, e := http.Get(args[0])
  if e != nil {
    fmt.Fprintf(os.Stderr, "ERROR: %v\n", e)
    os.Exit(-1)
  }

  c, e := ioutil.ReadAll(r.Body)
  r.Body.Close()
  if e != nil {
    fmt.Fprintf(os.Stderr, "ERROR: %v\n", e)
    os.Exit(-1)
  }

  fmt.Fprintf(os.Stdout, "%v\n", string(c))
  os.Exit(0)
}
