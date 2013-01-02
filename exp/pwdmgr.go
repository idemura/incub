package main

import (
  "os"
  "fmt"
  str "strings"
  "io/ioutil"
  fp "path/filepath"
  "bufio"
)

// pwdmgr --new (-n) <name> [password]
//  if no password, then asked to enter
// pwdmgr --gen (-g) <name> [--len %d]
//  automatically generate password of length n, default is 8
// pwdmgr --version (-v)
// pwdmgr --help
func main() {
  exit_code := parseCommand(os.Args[1:])
  os.Exit(exit_code)
}

func parseCommand(args []string) int {
  if len(args) == 0 {
    fmt.Fprintf(os.Stderr,
        "ERROR: Missing arguments.\nTry `pwdmgr --help` for more information.\n")
    return -1
  }

  switch args[0] {
  case "-n", "--new":
    fmt.Printf("new\n")
    return 0
  case "-g", "--gen":
    fmt.Printf("gen\n")
    return 0
  case "--version":
    fmt.Printf("pwdmgr 0.01\n")
    return 0
  case "--help":
    return help()
  }

  if args[0][0] == '-' {
    fmt.Fprintf(os.Stderr, "ERROR: name cannot start with -\n")
    return -1
  }
  return find(args[0])
}

func help() int {
  const text = `Password manager by Igor Demura
Usage:
  pwdmgr <name>
  pwdmgr --new <name> [password]
  pwdmgr --gen <name> [--len %i]

-n --new      set new password
-g --gen      generate new password of length 'len' (default is 8)
-l --list     list available sites
   --version  print version
   --help     usage information
`
  fmt.Print(text)
  return 0
}

func find(pattern string) int {
  entries, e := ioutil.ReadDir("pass")
  if e != nil {
    fmt.Fprintf(os.Stderr, "ERROR: Cannot read `pass` directory\n")
    return -1
  }

  exact_match := -1
  matches := make([]os.FileInfo, 0, 4)
  for _, fi := range entries {
    if !fi.IsDir() && str.Contains(fi.Name(), pattern) {
      if len(fi.Name()) == len(pattern) {
        exact_match = len(matches)
      }
      matches = append(matches, fi)
    }
  }

  switch {
  case len(matches) == 0:
    fmt.Fprintf(os.Stderr, "Files matching %v not found.\n", pattern)
    return -1
  case len(matches) == 1:
    return showPasswordFile(fp.Join("pass", matches[0].Name()))
  case exact_match >= 0:
    return showPasswordFile(fp.Join("pass", matches[exact_match].Name()))
  }

  fmt.Fprintf(os.Stderr, "Ambiguous files matching %v:\n", pattern)
  for _, fi := range matches {
    fmt.Fprintf(os.Stderr, "  %v\n", fi.Name())
  }
  return -1
}

func showPasswordFile(file_name string) int {
  f, e := os.Open(file_name)
  if e != nil {
    fmt.Fprintf(os.Stderr, "ERROR: Cannot file %v: %v\n",
        file_name, e.ToString())
    return -1
  }
  defer f.Close()

  br := bufio.NewBuffer()
  bs, e := br.ReadBytes('\n')
  if e != nil {
    return -1
  }
  fmt.Printf("value %v\n", bs)
  fmt.Printf("show file --%v--\n", file_name)
  fmt.Printf("User: %v\n", "idemura")
  fmt.Printf("Password: %v\n", "sv32x")
  return 0
}
