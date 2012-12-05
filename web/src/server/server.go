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
  "encoding/json"
)

type Config struct {
  Address string "address"
}

type server struct {
  baseDir string
  tplRoot, tplHttpError *tt.Template
  res []string
  cfg *Config
}

func template(name string) *tt.Template {
  return tt.Must(tt.ParseFiles(fp.Join("templates", name)))
}

func newConfig() *Config {
  return &Config{
      Address: "localhost:8080",
    }
}

func newServer(cfgPath string) *server {
  srv := new(server)

  srv.baseDir, _ = os.Getwd()
  log.Printf("Base path: %s\n", srv.baseDir)

  srv.cfg = newConfig()
  if len(cfgPath) > 0 {
    if f, e := os.Open(cfgPath); e == nil {
      if e := json.NewDecoder(f).Decode(srv.cfg); e != nil {
        log.Printf("ERROR JSON parse: %v", e)
      }
    } else {
      log.Printf("ERROR open config: %v", e)
    }
  }

  srv.tplRoot = template("root.html")
  srv.tplHttpError = template("httperror.html")

  fp.Walk("res",
    func (path string, fi os.FileInfo, e error) error {
      if !fi.IsDir() {
        srv.res = append(srv.res, path)
      }
      return nil
    })
  sort.Strings(srv.res)
  //log.Printf("%v", srv.res)

  return srv
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
  path := r.URL.Path
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

func (srv *server) run() {
  log.Printf("Server address: %s\n", srv.cfg.Address)
  http.ListenAndServe(srv.cfg.Address, srv)
}

func main() {
  log.SetFlags(log.Ltime)

  srv := newServer("config.json")
  srv.run()
}
