package main

import (
  "bufio"
//  "io"
  "os"
  "os/exec"
  "fmt"
  "net/http"
  "log"
  path "path/filepath"
  bs "bytes"
  uc "unicode"
//  tpl "http/template"
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

const (
  OpInsert = 0
  OpChange = 1
  OpDelete = 2
)

type Change struct {
  srcFirst, srcLast int
  dstFirst, dstLast int
  rem [][]byte
  ins [][]byte
  Op int
}

type Stream struct {
  p []byte
}

func CreateByteStream(bs []byte) *Stream {
  s := new(Stream)
  s.p = bs
  return s
}

func (s *Stream) Empty() bool {
  if len(s.p) == 0 {
    return true
  }
  for _, b := range s.p {
    if b != '\n' {
      return false
    }
  }
  return true
}

func (s *Stream) Length() int {
  return len(s.p)
}

func (s *Stream) PeekByte(j int) byte {
  return s.p[j]
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
  if len(s.p) > 0 && s.p[0] == b {
    s.p = s.p[1:]
    return true
  }
  return false
}

func (s *Stream) NextByte() byte {
  b := s.p[0]
  s.p = s.p[1:]
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
  text_op := s.NextByte()
  switch text_op {
  case 'a':
    c.Op = OpInsert
  case 'c':
    c.Op = OpChange
  case 'd':
    c.Op = OpDelete
  default:
    log.Fatalf("Unknown diff text op `%c`", text_op)
  }
  c.dstFirst, c.dstLast = GetRange(s)
  s.NextLine()

  if c.Op != OpInsert {
    c.srcLast += 1
    c.rem = make([][]byte, c.srcLast - c.srcFirst)
  }
  if c.Op != OpDelete {
    c.dstLast += 1
    c.ins = make([][]byte, c.dstLast - c.dstFirst)
  }

  for i := 0; i < len(c.rem); i++ {
    if s.IsByte('<') && s.IsByte(' ') {
      c.rem[i] = s.TakeLine()
    }
  }

  if s.Length() >= 3 &&
      s.PeekByte(0) == '-' &&
      s.PeekByte(1) == '-' &&
      s.PeekByte(2) == '-' {
    s.NextLine()
  }

  for i := 0; i < len(c.ins); i++ {
    if s.IsByte('>') && s.IsByte(' ') {
      c.ins[i] = s.TakeLine()
    }
  }

  return c
}

type ChangeVec []*Change

func NewChangeVector() ChangeVec {
  return make(ChangeVec, 0, 4)
}

func AddChange(c *Change, slice ChangeVec) ChangeVec {
  len_ := len(slice)
  cap_ := cap(slice)
  if (len_ == cap_) {
    new_slice := make(ChangeVec, len_, cap_ * 3 / 2)
    copy(new_slice, slice)
    slice = new_slice
  }
  slice = slice[0: len_ + 1]
  slice[len_] = c
  return slice
}

func ParseDiffOutput(text []byte) ChangeVec {
  changes := NewChangeVector()
  for s := CreateByteStream(text); !s.Empty(); {
    c := TakeChange(s)
    changes = AddChange(c, changes)
  }
  return changes
}

func WriteBufferToFile(file string, buf []bytes) error {
  f, e := os.Create(file)
  if e == nil {
    f.Write(buf.Bytes())
    f.Close()
  }
  return e
}

type CodeFormat struct {
  tabSize int
}

func (fmt *CodeFormat) FormatLine(s []byte) []byte {
  var buf = new(bs.Buffer)
  i := 0
  for k, b := range s {
    if uc.IsSpace(b) {

    } else {
    }
  }
}

func OutputModifications(srcFileName string, changes ChangeVec) {
  bs_src := bs.NewBuffer(nil)
  bs_mod := bs.NewBuffer(nil)

  f, e := os.Open(srcFileName)
  if e != nil {
    log.Printf("Can't open file: %v", srcFileName)
    return
  }

  reader := bufio.NewReader(f)
  ln_num := 0
  for _, c := range changes {
    for ; ln_num < c.srcFirst; {
      ln, _ := reader.ReadBytes('\n')
      ln_num += 1
      bs_src.Write(ln)
      bs_mod.Write(ln)
    }
    
    len_ins := len(c.ins)
    len_rem := len(c.rem)

    switch c.Op {
    case OpInsert:
      for _, s := range c.ins {
        bs_mod.Write([]byte("i>> "))
        bs_mod.Write(s)
      }
      for i := 0; i < len_ins; i++ {
        bs_src.Write([]byte("___\n"))
      }
    case OpChange:
      for _, s := range c.rem {
        bs_src.Write([]byte("c<< "))
        bs_src.Write(s)
      }
      for i := len_rem; i < len_ins; i++ {
        bs_src.Write([]byte("___\n"))
      }
      for _, s := range c.ins {
        bs_mod.Write([]byte("c>> "))
        bs_mod.Write(s)
      }
      for i := len_ins; i < len_rem; i++ {
        bs_mod.Write([]byte("___\n"))
      }
    case OpDelete:
      for _, s := range c.rem {
        bs_src.Write([]byte("d<< "))
        bs_src.Write(s)
      }
      for i := 0; i < len_rem; i++ {
        bs_mod.Write([]byte("___\n"))
      }
    }
    for ; ln_num < c.srcLast; {
      reader.ReadBytes('\n')
      ln_num += 1
    }
  }

  f.Close()

  WriteBufferToFile("x1.txt", bs_src)
  WriteBufferToFile("x2.txt", bs_mod)
}

func (self *Server) Diff(
    writer http.ResponseWriter,
    r *http.Request) {
  cmd := exec.Command("diff", "1.txt", "2.txt")
  out, err := cmd.Output()
  if err != nil {
    // Unfortunately, there is no platform independent way to get the exit code
    // in the error case: http://stackoverflow.com/questions/10385551/get-exit-code-go
    changes := ParseDiffOutput(out)
    OutputModifications("1.txt", changes)
    // fmt.Fprintf(writer, "<body><pre>%s</pre></body>\n", out)
  } else {
    // fmt.Fprintf(writer, "<body><h>No differences</h></body>\n")
  }
}

func (self *Server) Http404(
    writer http.ResponseWriter,
    r *http.Request) {
  writer.WriteHeader(http.StatusNotFound)
  fmt.Fprintf(writer,
      "<html><head></head><body><h1>404 - Not found</h1><p>%s</p></body></html>\n",
      r.URL.Path)
}

func (self *Server) ServeHTTP(
    writer http.ResponseWriter,
    r *http.Request) {
  log.Printf("Request path: %s", r.URL.Path)

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
  log.SetFlags(log.Ltime)

  cwd, _ := os.Getwd()
  log.Printf("Running in: %s", cwd)

  var srv = new(Server)
  srv.Init()

  srv.Diff(nil, nil)

  return

  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  http.ListenAndServe(address, srv)
}
