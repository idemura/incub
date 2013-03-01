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
  "errors"
  "flag"
  "time"
  fp "path/filepath"
  tt "text/template"
  "sort"
  "encoding/json"
  // cs "strings"
  "github.com/gorilla/sessions"
  "data"
)

type Config struct {
  Address string
  Debug bool
  DbUrl string
  GoogleApiKey string `json:"google_api_key"`
}

type PersonaResponse struct{
  Status string
  Email string
  Audience string
  Expires int64
  Issuer string
}

type server struct {
  baseDir string
  address string
  templates map[string]*tt.Template
  res []string
  sessionStore *sessions.CookieStore
}

var config = defaultCfg()

func defaultCfg() Config {
  return Config{
    Address: "localhost:8080",
    Debug: false,
    DbUrl: "localhost",
    GoogleApiKey: "",
  }
}

func (cfg *Config) read(fileName string) {
  log.Printf("Server config: %v", fileName)
  f, e := os.Open(fileName)
  if e != nil {
    log.Printf("ERROR open config: %v", e)
    return
  }

  defer f.Close()

  if e := json.NewDecoder(f).Decode(cfg); e != nil {
    log.Printf("ERROR JSON decode: %v", e)
  }
}

func newServer(address string) *server {
  srv := new(server)
  srv.address = address

  srv.baseDir, _ = os.Getwd()
  log.Printf("Base path: %s\n", srv.baseDir)

  srv.templates = make(map[string]*tt.Template)
  fs, _ := fp.Glob("templates/*.html")
  for _, f := range fs {
    t, e := tt.ParseFiles(f)
    if e != nil {
      log.Printf("ERROR: %v", e)
    } else {
      srv.templates[fp.Base(f)] = t
    }
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

func html(srv *server,
    rw io.Writer, file string, ctx interface{}) {
  if t, found := srv.templates[file]; found {
    e := t.Execute(rw, ctx)
    if e != nil {
      log.Printf("ERROR: %v", e)
    }
  } else {
    log.Printf("ERROR: Template %v not found", file)
  }
}

func formatPostTime(t time.Time) string {
  return t.Format("2006 Jan 02, 15:04")
}

func home(
    srv *server, rw http.ResponseWriter, r *http.Request,
    datactx *data.Context, user *data.User) {
  type Context struct {
    FormatTime func (time.Time) string
    User *data.User
    PostList data.PostList
  }

  ps := datactx.GetUserPosts(user)

  var ctx = Context{
    FormatTime: formatPostTime,
    User: user,
    PostList: ps,
  }
  html(srv, rw, "home.html", &ctx)
}

func auth(
    srv *server, rw http.ResponseWriter, r *http.Request) {
  html(srv, rw, "auth.html", nil)
}

func newPost(
    srv *server, rw http.ResponseWriter, r *http.Request,
    datactx *data.Context, user *data.User) {
  text := r.FormValue("text")
  if len(text) == 0 || len(text) > 180 {
    writeStatus(rw, 1)
    return
  }
  post := data.NewPost(user, time.Now(), text)
  datactx.SavePost(post)
  writeStatus(rw, 0)
}

func quit(srv *server) {
  log.Printf("Quit server")
  data.Close()

  os.Exit(0)
}

func newUserForm(
    srv *server, rw http.ResponseWriter, r *http.Request) {
  type Context struct {
    Email string
  }

  session := getSession(srv, r)
  email, found := getSessionStr(session, "email")
  if !found {
    httpError(srv, rw, 500, "Missing email session var in /newuserform")
    return
  }

  var ctx = Context{email}
  html(srv, rw, "newuserform.html", &ctx)
}

func checkUserForm(email string, r *http.Request) (*data.User, error) {
  firstName := r.FormValue("firstName")
  lastName := r.FormValue("lastName")
  userName := r.FormValue("userName")
  password := r.FormValue("password")
  if len(firstName) > 24 {
    return nil, errors.New("First name too long")
  }
  if len(lastName) > 24 {
    return nil, errors.New("Last name too long")
  }
  if len(userName) > 24 {
    return nil, errors.New("User name too long")
  }
  if len(password) > 16 {
    return nil, errors.New("Password too long")
  }
  user := data.NewUser(firstName, lastName, userName, email, password)
  return user, nil
}

func newUser(
    srv *server, rw http.ResponseWriter, r *http.Request) {
  session := getSession(srv, r)
  email, found := getSessionStr(session, "email")
  if !found {
    httpError(srv, rw, 500, "Missing email session var in /newuser")
    return
  }

  user, _ := checkUserForm(email, r)

  type Context struct {
    User *data.User
  }

  var ctx Context
  datactx := data.NewContext()
  if datactx.SaveUser(user) == nil {
    ctx.User = user
  }
  html(srv, rw, "newuser.html", &ctx)
}

func writeStatus(rw http.ResponseWriter, st int) {
  type Response struct {
    Status int
  }

  status := Response{st}
  buf, _ := json.Marshal(&status)
  rw.Write(buf)
}

func login(
    srv *server, rw http.ResponseWriter, r *http.Request) {
  resp, e := http.PostForm("https://verifier.login.persona.org/verify",
    url.Values{
      "assertion": {r.FormValue("assertion")},
      "audience": {srv.address},
    })
  if e != nil {
    writeStatus(rw, 3)
    return
  }
  body, _ := ioutil.ReadAll(resp.Body)
  resp.Body.Close()

  var persona PersonaResponse
  json.Unmarshal(body, &persona)
  if persona.Status != "okay" {
    writeStatus(rw, 2)
    return
  }

  // Put email in session in any case
  session := getSession(srv, r)
  session.Values["email"] = persona.Email
  session.Save(r, rw)

  datactx := data.NewContext()
  user := datactx.UserFromEmail(persona.Email)
  if user == nil {
    // Register email, until UserName set this record is invalid.
    datactx.SaveUser(data.NewBareUser(persona.Email))
    writeStatus(rw, 1)
  } else {
    writeStatus(rw, 0)
  }
}

func logout(
    srv *server, rw http.ResponseWriter, r *http.Request,
    datactx *data.Context, user *data.User) {
  session := getSession(srv, r)
  delete(session.Values, "email")
  session.Save(r, rw)

  type Response struct {
    Status int
  }

  var status Response
  buf, _ := json.Marshal(&status)
  rw.Write(buf)
}

var errorMsgMap = map[int]string {
    401: "Unauthorized",
    403: "Forbidden",
    404: "Not Found",
    500: "Internal Server Error",
  }

func httpError(
    srv *server, rw http.ResponseWriter,
    code int, description string) {
  type Context struct {
    Code int
    Message, Description string
  }

  log.Printf("%v: %v", code, description)
  rw.WriteHeader(code)

  if code == 401 {
    html(srv, rw, "unauthorized.html", nil)
    return
  }

  message, found := errorMsgMap[code]
  if !found {
    message = "Unknown"
  }

  ctx := Context{code, message, description}
  html(srv, rw, "httperror.html", &ctx)
}

func serveFile(
    srv *server, rw http.ResponseWriter, r *http.Request,
    path string) bool {
  fullPath := fp.Join("res", path)
  i := sort.SearchStrings(srv.res, fullPath)
  if i < len(srv.res) && srv.res[i] == fullPath {
    http.ServeFile(rw, r, fullPath)
    return true
  }
  return false
}

func (srv *server) ServeHTTP(
    rw http.ResponseWriter, r *http.Request) {
  path := r.URL.Path

  datactx := data.NewContext()
  var user *data.User = nil
  session := getSession(srv, r)
  if session != nil {
    email, found := getSessionStr(session, "email")
    if (found) {
      // Check user email in my base
      user = datactx.UserFromEmail(email)
      if user.UserName() == "" {
        user = nil
      }
    }
  }

  if path == "/quit" && config.Debug {
    quit(srv)
    return
  // } else if path == "/js/script.js" {
  //   //http.ServeFile(http, r, path)
  //   return
  }

  if user == nil {
    if path == "/" {
      auth(srv, rw, r)
    } else if path == "/login" {
      login(srv, rw, r)
    } else if path == "/newuserform" {
      newUserForm(srv, rw, r)
    } else if path == "/newuser" {
      newUser(srv, rw, r)
    } else if !serveFile(srv, rw, r, path) {
      auth(srv, rw, r)
    }
  } else {
    if path == "/" {
      home(srv, rw, r, datactx, user)
    } else if path == "/logout" {
      logout(srv, rw, r, datactx, user)
    } else if path == "/newpost" {
      newPost(srv, rw, r, datactx, user)
    } else if !serveFile(srv, rw, r, path) {
      httpError(srv, rw, 404, path)
    }
  }
}

func run(srv *server) {
  log.Printf("Server address: %s\n", srv.address)
  http.ListenAndServe(srv.address, srv)
}

func getSession(srv *server, r *http.Request) *sessions.Session {
  s, _ := srv.sessionStore.Get(r, "tapecoll")
  return s
}

func main() {
  log.SetFlags(log.Ltime | log.Lmicroseconds)

  configFile := flag.String("config", "config.json", "server config file")
  flag.Parse()

  config.read(*configFile)
  if config.Debug {
    log.Printf("DEBUG mode")
  }

  if len(os.Args) > 1 && os.Args[1] == "-dbinit" {
    data.Init(config.DbUrl)
    return
  }

  if !data.Open(config.DbUrl) {
    data.Close()
    return
  }

  srv := newServer(config.Address)
  run(srv)
}
