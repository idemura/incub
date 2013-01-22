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
  "io/ioutil"
  "os"
  "io"
  "net/http"
  "net/url"
  "log"
  fp "path/filepath"
  tt "text/template"
  "sort"
  "encoding/json"
  // cs "strings"
  "github.com/gorilla/sessions"
  "data"
)

type Config struct {
  Address string "address"
  Debug bool "debug"
}

type PersonaResponse struct{
  Status string "status"
  Email string "email"
  Audience string "audience"
  Expires int64 "expires"
  Issuer string "issuer"
}

type server struct {
  baseDir string
  templates map[string]*tt.Template
  res []string
  cfg *Config
  sessionStore *sessions.CookieStore
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

  fs, _ := fp.Glob("templates/*.html")
  srv.templates = make(map[string]*tt.Template)
  for _, f := range fs {
    srv.templates[fp.Base(f)] = tt.Must(tt.ParseFiles(f))
  }

  srv.sessionStore = sessions.NewCookieStore([]byte("xMjgtU1v67c0"))

  fp.Walk("res",
    func (path string, fi os.FileInfo, e error) error {
      if !fi.IsDir() {
        srv.res = append(srv.res, path)
      }
      return nil
    })
  sort.Strings(srv.res)

  return srv
}

func getSessionStr(s *sessions.Session, name string) (string, bool) {
  val, found := s.Values[name]
  if str, ok := val.(string); found && ok {
    return str, true
  }
  return "", false
}

func (srv *server) html(wr io.Writer,
    file string, ctx interface{}) {
  if t, e := srv.templates[file]; e {
    t.Execute(wr, ctx)
  }
}

func (srv *server) home(
    writer http.ResponseWriter, r *http.Request) {
  type UserCtx struct {
    Email string
  }
  type HomeCtx struct {
    User *UserCtx
  }

  var ctx HomeCtx
  session := srv.getSession(r)
  if session != nil {
    email, found := getSessionStr(session, "email")
    if (found) {
      // Check user email in my base
      datactx := data.NewDataCtx()
      user := datactx.UserByEmail(email)
      if user != nil {
        ctx.User = &UserCtx{user.Email}        
      }
    }
  }
  srv.html(writer, "home.html", &ctx)
}

func (srv *server) quit(
    writer http.ResponseWriter, r *http.Request) {
  log.Printf("Exiting server")
  data.Uninit()
  os.Exit(0)
}

func (srv *server) newUserForm(
    writer http.ResponseWriter, r *http.Request) {
  type Context struct {
    Email string
  }

  session := srv.getSession(r)
  email, found := getSessionStr(session, "email")
  if !found {
    srv.error(writer, 500, "Missing email session var in /newuserform")
    return
  }

  var ctx = Context{email}
  srv.html(writer, "newuserform.html", &ctx)
}

func (srv *server) newUser(
    writer http.ResponseWriter, r *http.Request) {
  session := srv.getSession(r)
  email, found := getSessionStr(session, "email")
  if !found {
    srv.error(writer, 500, "Missing email session var in /newuser")
    return
  }

  user := data.User{
    FirstName: r.FormValue("firstName"),
    LastName: r.FormValue("lastName"),
    UserName: r.FormValue("userName"),
    Email: email,
    Password: r.FormValue("password"),
  }

  type Context struct {
    User *data.User
  }

  var ctx Context
  datactx := data.NewDataCtx()
  if datactx.NewUser(&user) {
    ctx.User = &user
  }
  srv.html(writer, "newuser.html", &ctx)
}

func (srv *server) login(
    writer http.ResponseWriter, r *http.Request) {
  type Response struct {
    Status int
    UserId string
  }

  var status Response

  resp, e := http.PostForm("https://verifier.login.persona.org/verify",
    url.Values{
      "assertion": {r.FormValue("assertion")},
      "audience": {srv.cfg.Address},
    })
  if e != nil {
    status.Status = 3
    buf, _ := json.Marshal(&status)
    writer.Write(buf)
    return
  }
  body, _ := ioutil.ReadAll(resp.Body)
  resp.Body.Close()

  var persona PersonaResponse
  json.Unmarshal(body, &persona)
  if persona.Status != "okay" {
    status.Status = 2
    buf, _ := json.Marshal(&status)
    writer.Write(buf)
    return
  }

  // Put email in session in any case
  session := srv.getSession(r)
  session.Values["email"] = persona.Email
  session.Save(r, writer)

  datactx := data.NewDataCtx()
  user := datactx.UserByEmail(persona.Email)
  if user == nil {
    status.Status = 1
    buf, _ := json.Marshal(&status)
    writer.Write(buf)
  } else {
    status.Status = 0
    status.UserId = "demi"
    buf, _ := json.Marshal(&status)
    writer.Write(buf)
  }
}

func (srv *server) logout(
    writer http.ResponseWriter, r *http.Request) {
  session := srv.getSession(r)
  delete(session.Values, "email")
  session.Save(r, writer)
  
  type Response struct {
    Status int
  }

  var status Response
  buf, _ := json.Marshal(&status)
  writer.Write(buf)
}

var errorMsgMap = map[int]string {
    404: "Page Not Found",
    500: "Internal Server Error",
  }

func (srv *server) error(
    writer http.ResponseWriter, code int, description string) {
  type Context struct {
    Code int
    Message, Description string
  }

  if code >= 500 {
    log.Printf("%v: %v", code, description)
  }
  writer.WriteHeader(code)
  message, found := errorMsgMap[code]
  if !found {
    message = "Unknown"
  }

  srv.html(writer, "httperror.html",
      &Context{code, message, description})
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
    srv.home(writer, r)
  } else if path == "/quit" {
    if srv.cfg.Debug {
      srv.quit(writer, r)
    }
  } else if path == "/login" {
    srv.login(writer, r)
  } else if path == "/logout" {
    srv.logout(writer, r)
  } else if path == "/newuserform" {
    srv.newUserForm(writer, r)
  } else if path == "/newuser" {
    srv.newUser(writer, r)
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

func (srv *server) getSession(r *http.Request) *sessions.Session {
  s, _ := srv.sessionStore.Get(r, "tapecoll")
  return s
}

func main() {
  log.SetFlags(log.Ltime | log.Lmicroseconds)

  if !data.Init("localhost") {
    data.Uninit()
    return
  }

  if len(os.Args) > 1 && os.Args[1] == "--dbinit" {
    log.Printf("DB init")
    log.Printf("DB init DONE")
    return
  }

  srv := newServer("config.json")
  srv.run()
}
