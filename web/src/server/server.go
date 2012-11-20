package main

import (
//  "bufio"
  "os"
  "fmt"
  "net/http"
  "log"
  path "path/filepath"
  exec "os/exec"
  // bs "bytes"
  uc "unicode"
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

type Change struct {
  srcFirst, srcLast int
  dstFirst, dstLast int
  rem [][]byte
  ins [][]byte
}

type Stream struct {
  p []byte
}

func CreateByteStream(bs []byte) *Stream {
  s := new(Stream)
  s.p = bs
  return s
}

func (s *Stream) Eof() bool {
  return len(s.p) == 0
}

func (s *Stream) NextLine() {
  for i := 0; i < len(s.p); i++ {
    if s.p[i] == '\n' {
      s.p = s.p[i + 1:]
      break
    }
  }
}

func (s *Stream) IsByte(b byte) bool {
  if s.p[0] == b {
    s.p = s.p[1: ]
    return true;
  }
  return false;
}

func (s *Stream) NextByte() byte {
  b := s.p[0]
  s.p = s.p[1: ]
  return b
}

func (s *Stream) TakeLine() []byte {
  for i := 0; i < len(s.p); i++ {
    if s.p[i] == '\n' {
      line := s.p[:i + 1]
      s.p = s.p[i + 1:]
      return line
    }
  }
  line := s.p
  s.p = nil
  return line
}

func (s *Stream) TakeInt() int {
  var i = 0
  var n = 0
  for ; i < len(s.p); i++ {
    if uc.IsDigit(rune(s.p[i])) {
      n = 10 * n + int(s.p[i] - '0')
    } else {
      s.p = s.p[i:]
      return n
    }
  }
  s.p = nil
  return n
}

func GetRange(s *Stream) (first, last int) {
  first = s.TakeInt()
  if s.IsByte(',') {
    last = s.TakeInt()
  } else {
    last = first
  }
  return
}

func TakeChange(s *Stream) *Change {
  c := new(Change)
  c.srcFirst, c.srcLast = GetRange(s)
  s.NextByte()
  c.dstFirst, c.dstLast = GetRange(s)
  s.NextLine()

  c.rem = make([][]byte, c.srcLast - c.srcFirst + 1);
  c.ins = make([][]byte, c.dstLast - c.dstFirst + 1);

  for i := 0; i <= len(c.rem); i++ {
    if s.IsByte('<') && s.IsByte(' ') {
      c.rem[i] = s.TakeLine()
    }
  } 

  return c
}

func ProcessDiffOutput(text []byte) {
  // //var prefix = []byte("> ")
  // var isQuote = func(s []byte) bool {
  //   l := len(s)
  //   switch {
  //     case l == 0:
  //       return false
  //     case s[0] == '>' || s[0] == '<':
  //       return true
  //     case s[0] == '-' && l >= 3 && s[1] == '-' && s[2] == '-':
  //       return true
  //   }
  //   return false;
  // }
  // for ln, text := GetLine(text); ln != nil; ln, text = GetLine(text) {
  //   if !isQuote(ln) {
  //     log.Printf("%v -- %s", len(ln), ln)
  //   }
  // }

  log.Printf("%s", text[:10])
  s := CreateByteStream(text)
  c := TakeChange(s)
  log.Printf("%v %v - %v %v", c.srcFirst, c.srcLast, c.dstFirst, c.dstLast)

  // f, e := os.Open("1.txt")
  // if e != nil {
  //   log.Printf("IO error")
  //   return
  // }

  // br := bufio.NewReader(f)
  // for buf, e := br.ReadBytes('\n');
  //     e == nil; buf, e = br.ReadBytes('\n') {
  //   log.Printf("%s", buf)
  // }
}

func (self *Server) Diff(
    writer http.ResponseWriter,
    r *http.Request) {
  cmd := exec.Command("diff", "1.txt", "2.txt")
  out, err := cmd.Output()
  if err != nil {
    // Unfortunately, there is no platform independent way to get the exit code
    // in the error case: http://stackoverflow.com/questions/10385551/get-exit-code-go
    ProcessDiffOutput(out)
    // fmt.Fprintf(writer, "<body><pre>%s</pre></body>\n", out);
  } else {
    // fmt.Fprintf(writer, "<body><h>No differences</h></body>\n");
  }
}

func (self *Server) Http404(
    writer http.ResponseWriter,
    r *http.Request) {
  writer.WriteHeader(http.StatusNotFound)
  fmt.Fprintf(writer,
      "<html><head></head><body><h1>404 - Not found</h1><p>%s</p></body></html>\n",
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
  cwd, _ := os.Getwd()
  log.Printf("Running in: %s", cwd);

  var srv = new(Server)
  srv.Init()

  srv.Diff(nil, nil)

  return

  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  http.ListenAndServe(address, srv)
}
