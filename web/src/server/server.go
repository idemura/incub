package main

import (
  // "bufio"
  // "io"
  "os"
  // "os/exec"
  "fmt"
  "net/http"
  "log"
  fp "path/filepath"
  // bs "bytes"
  // uc "unicode"
  tt "html/template"
)

type server struct {
  baseDir string
  tplRoot, tplHttpError *tt.Template
  res []string
}

func loadTpl(name string) *tt.Template {
  return tt.Must(tt.ParseFiles(fp.Join("templates", name)))
}

func (srv *server) init() {
  srv.baseDir, _ = os.Getwd()
  srv.tplRoot = loadTpl("root.html")
  srv.tplHttpError = loadTpl("httperror.html")

  fp.Walk("res/",
    func (path string, fi os.FileInfo, e error) error {
      if !fi.IsDir() {
        srv.res = append(srv.res, "/" + fp.Base(path))
      }
      return nil
    })
  //log.Printf("%v", srv.res)
}

func (srv *server) root(
    writer http.ResponseWriter,
    r *http.Request) {
  srv.tplRoot.Execute(writer, nil)
}

type HttpErrorTplCtx struct {
  ErrId int
  ErrString, Description string
}

func (srv *server) http404(
    writer http.ResponseWriter,
    r *http.Request) {
  writer.WriteHeader(http.StatusNotFound)
  c := HttpErrorTplCtx{404, "Not Found", fmt.Sprintf("Path %v", r.URL.Path)}
  srv.tplHttpError.Execute(writer, &c)
}

func (srv *server) getStatic(path string) string {
  for _, f := range srv.res {
    if f == path {
      return f
    }
  }
  return ""
}

func (srv *server) ServeHTTP(
    writer http.ResponseWriter,
    r *http.Request) {
  log.Printf("Request path: %s", r.URL.Path)

  var path = r.URL.Path
  if path == "/" {
    srv.root(writer, r)
  // } else if path == "/diff" {
  } else if f := srv.getStatic(path); len(f) != 0 {
    log.Printf("%v", f)
    http.ServeFile(writer, r, f)
  } else {
    log.Printf("404: %v", path)
    srv.http404(writer, r)
  }
}

func main() {
  log.SetFlags(log.Ltime)

  var srv = new(server)
  srv.init()
  log.Printf("Running in: %s", srv.baseDir)

  const address = "localhost:8080"
  fmt.Printf("Server started at %s\n", address)
  http.ListenAndServe(address, srv)
}
