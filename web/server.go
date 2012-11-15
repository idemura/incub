package main

import (
  "io"
  "fmt"
  "net/http"
  "log"
)

type Server struct {
  // State
}

func TrimTrailingSplases(s string) string {
  l := len(s)
  if l == 0 {
    return s
  }

  for i := l - 1; i > 0; i-- {
    if s[i] != '/' {
      return s[: i + 1]
    }
  }
  return ""
}

func (h *Server) ServeHTTP(
    w http.ResponseWriter,
    r *http.Request) {
  log.Printf("Request path: %s", r.URL.Path);
}

func main() {
  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  var srv = new(Server)
  http.ListenAndServe(address, srv)
}
