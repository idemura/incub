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
  "bytes"
  "log"
  "encoding/json"
  fp "path/filepath"
  lv "github.com/jmhodges/levigo"
)

type dbMap struct {
  name string
  db *lv.DB
}

type dbEnv struct {
  users dbMap
  user_emails dbMap
  wo *lv.WriteOptions
  ro *lv.ReadOptions
}

var dbe dbEnv

func (dbe *dbEnv) init() bool {
  dbe.wo = lv.NewWriteOptions()
  dbe.ro = lv.NewReadOptions()

  const K = 1024

  if !dbe.users.open(fp.Join("ldb", "users"), 160*K, 0) {
    return false
  }
  if !dbe.user_emails.open(fp.Join("ldb", "user_emails"), 120*K, 0) {
    return false
  }

  return true
}

func (dbe *dbEnv) deinit() {
  dbe.users.close()
  dbe.user_emails.close()
  if dbe.ro != nil {
    dbe.ro.Close()
    dbe.ro = nil
  }
  if dbe.wo != nil {
    dbe.wo.Close()
    dbe.wo = nil
  }
}

func (dbm *dbMap) open(name string, cache, block int) bool {
  opts := lv.NewOptions()
  if cache > 0 {
    opts.SetCache(lv.NewLRUCache(cache))
  }
  if block > 0 {
    opts.SetBlockSize(block)
  }
  opts.SetCreateIfMissing(true)
  defer opts.Close()

  dbm.name = name
  if db, e := lv.Open(name, opts); e != nil {
    log.Printf("DB ERROR in open name=%v: %v", name, e)
    return false
  } else {
    dbm.db = db
  }

  return true
}

func (dbm *dbMap) close() {
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

func initDB() bool {
  if !dbe.init() {
    log.Printf("DB ERROR: Fail to init")
    dbe.deinit()
    return false
  }
  return true
}

func deinitDB() {
  dbe.deinit()
}

type dbUser struct {
  id []byte
  firstName string
  lastName string
  userName string
  email string
  password string
}

func dbUserByEmail(email []byte) *dbUser {
  val := dbe.user_emails.get([]byte(email))
  if val != nil {
    var user dbUser
    json.NewDecoder(bytes.NewBuffer(val)).Decode(&user)
    return &user
  }
  return nil
}

func dbNewUser(email, firstName, lastName string) bool {
  // TODO
  return false
}
