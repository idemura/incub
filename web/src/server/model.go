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
// along with TapeColl. If not, see <http://www.gnu.org/licenses/>.
package main

import (
  "log"
  // "encoding/json"
  fp "path/filepath"
  lv "github.com/jmhodges/levigo"
)

type dbMap struct {
  name string
  db *lv.DB
}

type dbEnv struct {
  wo *lv.WriteOptions
  ro *lv.ReadOptions
}

var dbe *dbEnv

func openMap(name string, cache, block int) dbMap {
  opts := lv.NewOptions()
  if cache > 0 {
    opts.SetCache(lv.NewLRUCache(cache))
  }
  if block > 0 {
    opts.SetBlockSize(block)
  }
  opts.SetCreateIfMissing(true)
  defer opts.Close()

  var e error
  var dbm dbMap = dbMap{name: name}
  dbm.db, e = lv.Open(fp.Join("ldb", "user"), opts)
  if e != nil {
    log.Printf("DB ERROR in openMap name=%v: %v", name, e)
  }

  return dbm
}

func (dbm dbMap) close() {
  if dbm.db != nil {
    dbm.db.Close()
    dbm.db = nil
  }
}

func (dbm *dbMap) get(k []byte) []byte {
  v, e := dbm.db.Get(dbe.ro, k)
  if e != nil {
    log.Printf("DB ERROR in get: %v", e)
    return nil
  }
  return v
}

func (dbm *dbMap) put(k, v []byte) {
  e := dbm.db.Put(dbe.wo, k, v)
  if e != nil {
    log.Printf("DB ERROR in put: %v", e)
  }
}

func initDB() {
  if dbe != nil {
    return
  }

  dbe = new(dbEnv)

  // var e error
  // dbc.db, e = lv.Open(fp.Join("ldb", "user"), opts)
  // if e != nil {
  //   log.Printf("DB ERROR in newDB: %v", e)
  //   return nil
  // }

  dbe.wo = lv.NewWriteOptions()
  dbe.ro = lv.NewReadOptions()

  // val := make([]byte, 4)
  // val[0] = 1
  // val[1] = 0
  // val[2] = 0
  // val[3] = 0

  // v, _ := dbc.db.Get(dbc.ro, []byte("igord"))
  // log.Printf("%v", v)

  // dbc.db.Put(dbc.wo, []byte("igord"), val)
  // log.Printf("Ura!")

  // return &dbc
}

func finalizeDB() {
  if dbe == nil {
    return
  }
  if dbe.ro != nil {
    dbe.ro.Close()
    dbe.ro = nil
  }
  if dbe.wo != nil {
    dbe.wo.Close()
    dbe.wo = nil
  }
}

type dbUser struct {
  id []byte
  firstName string
  lastName string
  userName string
  email string
  password string
}

func readUser(dbm dbMap, name string) *dbUser {
  var user dbUser
  return &user
}
