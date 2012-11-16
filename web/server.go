package main

import (
  // "io"
  // "os"
  "fmt"
  "net/http"
  "log"
  path "path/filepath"
  exec "os/exec"
)

type Server struct {
  BasePath string
}

func (self *Server) Init() {
  self.BasePath, _ = path.Abs(".")
}

func (self *Server) Root(
    writer http.ResponseWriter,
    r *http.Request) {
  fmt.Fprintf(writer, "Hi there, I love Go!")
}

func (self *Server) Diff(
    writer http.ResponseWriter,
    r *http.Request) {
  cmd := exec.Command("diff", "1.txt", "2.txt")
  out, err := cmd.Output()
  if err != nil {
    // Unfortunately, there is no platform independent way to get the exit code
    // in the error case: http://stackoverflow.com/questions/10385551/get-exit-code-go
    fmt.Fprintf(writer, "<body><p>%s</p></body>\n", out);
  } else {
    fmt.Fprintf(writer, "<body><h>No differences</h></body>\n");
  }
}

func (self *Server) Http404(
    writer http.ResponseWriter,
    r *http.Request) {
  // TODO:
  fmt.Fprintf(writer,
      "<body><h1>404 - Not found</h1><p>%s</p></body>\n",
      r.URL.Path);
}

func (self *Server) ServeHTTP(
    writer http.ResponseWriter,
    r *http.Request) {
  log.Printf("Request path: %s", r.URL.Path);
  
  var path = r.URL.Path
  if path == "/" {
    self.Root(writer, r)
  } else if path == "/diff" {
    self.Diff(writer, r)
  } else {
    self.Http404(writer, r)
  }
}

func main() {
  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  var srv = new(Server)
  srv.Init()
  http.ListenAndServe(address, srv)
}
