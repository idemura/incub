package main

import (
  bs "bytes"
)

func GetLine(text []byte) (line []byte, rest []byte) {
  if text == nil {
    line = nil
    rest = nil
    return
  }
  if j := bs.IndexByte(text, '\n'); j < 0 {
    line = text
    rest = nil
  } else {
    line = text[:j + 1]
    rest = text[j + 1:]
  }
  return
}
