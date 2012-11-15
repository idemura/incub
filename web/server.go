package main

import (
  "io"
  "fmt"
  "net/http"
  "log"
  re "regexp"
)

type Server struct {
  writer http.ResponseWriter
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

func (self *Server) Root() {
  fmt.Fprintf(self.writer, "Hi there, I love Go!")
}

func (self *Server) Page404() {
  fmt.Fprintf(self.writer, "<h1>404 - Not found</h1><p>%s</p>\n" 
}

func (self *Server) ServeHTTP(
    w http.ResponseWriter,
    r *http.Request) {
  self.writer = w

  log.Printf("Request path: %s", r.URL.Path);
  var path = r.URL.path
  if path == "/" {
    self.Root()
  } else {
    fmt.Fprintf(self.wr
  }
}

func main() {
  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  var srv = new(Server)
  srv.InitRoutes()
  http.ListenAndServe(address, srv)
}
