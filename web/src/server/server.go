// Copyright 2012 Igor Demura
//
// This file is part of Incub.
//
// TapeColl is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// TapeColl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with TapeColl. If not, see <http://www.gnu.org/licenses/>.
package main

import (
  // "bufio"
  "bytes"
  // "io"
  "io/ioutil"
  "os"
  // "fmt"
  "net/http"
  "net/url"
  "log"
  fp "path/filepath"
  tt "html/template"
  "sort"
  "encoding/json"
  // cs "strings"
  "github.com/gorilla/sessions"
)

type Config struct {
  Address string "address"
  Debug bool "debug"
}

type PersonaAuthReply struct{
  Status string "status"
  Email string "email"
  Audience string "audience"
  Expires int64 "expires"
  Issuer string "issuer"
}

type server struct {
  baseDir string
  tplRoot, tplHttpError, tplQuit, tplNewUser *tt.Template
  res []string
  cfg *Config
  session_store *sessions.CookieStore
}

func template(name string) *tt.Template {
  return tt.Must(tt.ParseFiles(fp.Join("templates", name)))
}

func newConfig() *Config {
  return &Config{
      Address: "localhost:8080",
      Debug: false,
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
      f.Close()
    } else {
      log.Printf("ERROR open config: %v", e)
    }
  }
  if srv.cfg.Debug {
    log.Printf("Running in debug mode");
  }

  srv.tplRoot = template("root.html")
  srv.tplHttpError = template("httperror.html")
  srv.tplQuit = template("quit.html")
  srv.tplNewUser = template("newuser.html")

  srv.session_store = sessions.NewCookieStore([]byte("tapecoll by Igor Demura"))

  fp.Walk("res",
    func (path string, fi os.FileInfo, e error) error {
      if !fi.IsDir() {
        srv.res = append(srv.res, path)
      }
      return nil
    })
  sort.Strings(srv.res)
  // log.Printf("%v", srv.res)

  return srv
}

type RootCtx struct {
  UserEmail string
}

func (srv *server) root(
    writer http.ResponseWriter, r *http.Request) {
  c := RootCtx{"null"}
  srv.tplRoot.Execute(writer, &c)
}

func (srv *server) quit(
    writer http.ResponseWriter, r *http.Request) {
  deinitDB()
  log.Printf("Quit server")
  os.Exit(0)
}

func (srv *server) newuser(
    writer http.ResponseWriter, r *http.Request) {
  srv.tplNewUser.Execute(writer, nil)
}

func (srv *server) login(
    writer http.ResponseWriter, r *http.Request) {
  type AuthStatus struct {
    Status int
    UserId string
  }

  var auth_status AuthStatus

  resp, e := http.PostForm("https://verifier.login.persona.org/verify",
    url.Values{
      "assertion": {r.FormValue("assertion")},
      "audience": {srv.cfg.Address},
    })
  if e != nil {
    auth_status.Status = 3
    buf, _ := json.Marshal(&auth_status)
    writer.Write(buf)
    return
  }
  body, _ := ioutil.ReadAll(resp.Body)
  resp.Body.Close()

  var persona PersonaAuthReply
  json.NewDecoder(bytes.NewBuffer(body)).Decode(&persona)
  if persona.Status != "okay" {
    auth_status.Status = 2
    buf, _ := json.Marshal(&auth_status)
    writer.Write(buf)
    return
  }

  session, _ := srv.session_store.Get(r, "tapecoll")
  session.Values["email"] = persona.Email
  session.Save(r, writer)

  user := dbUserByEmail([]byte(persona.Email))
  if user == nil {
    auth_status.Status = 1
    buf, _ := json.Marshal(&auth_status)
    writer.Write(buf)
  } else {
    auth_status.Status = 0
    auth_status.UserId = "demi"
    buf, _ := json.Marshal(&auth_status)
    writer.Write(buf)
  }
}

type HttpErrorCtx struct {
  ErrId int
  ErrString, Description string
}

var error_codes = map[int]string {
    404: "Page Not Found",
    500: "Internal Server Error",
  }

func (srv *server) error(
    writer http.ResponseWriter, err_code int, description string) {
  if err_code >= 500 {
    log.Printf("%v: %v", err_code, description)
  }
  writer.WriteHeader(err_code)
  err_string, found := error_codes[err_code]
  if !found {
    err_string = "Unknown"
  }
  srv.tplHttpError.Execute(writer, &HttpErrorCtx{err_code, err_string,
      description})
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
    writer http.ResponseWriter, r *http.Request) {
  path := r.URL.Path
  if path == "/" {
    srv.root(writer, r)
  } else if path == "/quit" {
    if srv.cfg.Debug {
      srv.quit(writer, r)
    }
  } else if path == "/login" {
    srv.login(writer, r)
  } else if path == "/newuser" {
    srv.newuser(writer, r)
  } else if f := srv.getStatic(path); len(f) != 0 {
    http.ServeFile(writer, r, f)
  } else {
    log.Printf("404: %v", path)
    srv.error(writer, 404, path)
  }
}

func (srv *server) run() {
  log.Printf("Server address: %s\n", srv.cfg.Address)
  http.ListenAndServe(srv.cfg.Address, srv)
}

func main() {
  log.SetFlags(log.Ltime | log.Lmicroseconds | log.Lshortfile)

  initDB()

  srv := newServer("config.json")
  srv.run()
}
