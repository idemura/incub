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
  "sort"
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

  fp.Walk("res",
    func (path string, fi os.FileInfo, e error) error {
      if !fi.IsDir() {
        srv.res = append(srv.res, path)
      }
      return nil
    })
  sort.Strings(srv.res)
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
  c := HttpErrorTplCtx{404, "Not Found",
      fmt.Sprintf("Requested %v", r.URL.Path)}
  srv.tplHttpError.Execute(writer, &c)
}

func (srv *server) getStatic(path string) string {
  path = fp.Join("res", path)
  i := sort.SearchStrings(srv.res, path)
  if i < len(srv.res) && srv.res[i] == path {
    return path
  } else {
    return ""
  }
  return "" // Workaround Go compiler error
}

func (srv *server) ServeHTTP(
    writer http.ResponseWriter,
    r *http.Request) {
  var path = r.URL.Path
  if path == "/" {
    srv.root(writer, r)
  // } else if path == "/diff" {
  } else if f := srv.getStatic(path); len(f) != 0 {
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
  log.Printf("Base path: %s\n", srv.baseDir)

  const address = "localhost:8080"
  log.Printf("Server address: %s\n", address)
  http.ListenAndServe(address, srv)
}
