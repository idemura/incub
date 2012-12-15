// This file is part of TapeColl.
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
// along with Foobar. If not, see <http://www.gnu.org/licenses/>.
package main

import (
  bs "bytes"
)

func GetByteLine(text []byte) (line []byte, rest []byte) {
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
