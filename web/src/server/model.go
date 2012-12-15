package main

import (
  "log"
  "encoding/json"
  fp "path/filepath"
  lv "github.com/jmhodges/levigo"
)

var wo *lv.WriteOptions
var ro *lv.ReadOptions

type dbMap struct {
  name string
  db *lv.DB
}

func openMap(name string, cache, block int) dbMap {
  var dbm dbMap
  opts := lv.NewOptions()
  if cache != 0 {
  opts.SetCache(lv.NewLRUCache(160*1024))
  opts.SetBlockSize(16*1024)
  opts.SetCreateIfMissing(true)

  var e error
  dbc.db, e = lv.Open(fp.Join("ldb", "user"), opts)
  if e != nil {
    log.Printf("DB ERROR in newDB: %v", e)
    return nil
  }
  opts.Close()
}

type dbConn struct {
  db *lv.DB
}

type dbUser struct {
  id []byte
  firstName string
  lastName string
  userName string
  email string
  password string
}

func (dbc *dbConn) closeDB() {
  if dbc.wo != nil {
    dbc.wo.Close()
    dbc.wo = nil
  }
  if dbc.ro != nil {
    dbc.ro.Close()
    dbc.ro = nil
  }
  if dbc.db != nil {
    dbc.db.Close()
    dbc.db = nil
  }
}

func (dbc *dbConn) get(k []byte) []byte {
  v, e := dbc.db.Get(dbc.ro, k)
  if e != nil {
    log.Printf("DB ERROR in get: %v", e)
    return nil
  }
  return v
}

func (dbc *dbConn) put(k, v []byte) {
  e := dbc.db.Put(k, b)
  if e != nil {
    log.Printf("DB ERROR in put: %v", e)
  }
}

func newDB() *dbConn {
  var dbc dbConn

  opts := lv.NewOptions()
  opts.SetCache(lv.NewLRUCache(40*1024*1024))
  opts.SetBlockSize(16*1024)
  opts.SetCreateIfMissing(true)

  var e error
  dbc.db, e = lv.Open(fp.Join("ldb", "user"), opts)
  if e != nil {
    log.Printf("DB ERROR in newDB: %v", e)
    return nil
  }
  opts.Close()

  dbc.wo = lv.NewWriteOptions()
  dbc.ro = lv.NewReadOptions()

  val := make([]byte, 4)
  val[0] = 1
  val[1] = 0
  val[2] = 0
  val[3] = 0

  v, _ := dbc.db.Get(dbc.ro, []byte("igord"))
  log.Printf("%v", v)

  dbc.db.Put(dbc.wo, []byte("igord"), val)
  log.Printf("Ura!")

  return &dbc
}

func (dbc *dbConn) readUser(string ) *dbUser {
  var user dbUser
  v := dbc.get(
  return &user
}
