package main

import (
  "os"
  "fmt"
)

func emptyArgs() int {
  fmt.Fprintf(os.Stderr,
    "ERROR: Missing arguments.\nTry `pwdmgr --help` for more information.\n")
  return -1
}

func parseCommand(args []string) int {
  if len(args) == 0 {
    return emptyArgs()
  }

  switch args[0] {
  case "-n", "--new":
    fmt.Printf("new\n")
    return 0
  case "-g", "--gen":
    fmt.Printf("gen\n")
    return 0
  case "--version":
    return version()
  }

  return 0
}

// pwdmgr --new (-n) <name> [password]
//  if no password, then asked to enter
// pwdmgr --gen (-g) <name> [len=n]
//  automatically generate password of length n, default is 8
// pwdmgr --version (-v)
// pwdmgr --help
func main() {
  exit_code := parseCommand(os.Args[1:])
  os.Exit(exit_code)
}
