package main

import (
  "log"
  fp "path/filepath"
  lv "github.com/jmhodges/levigo"
)

type dbConn struct {
  db *lv.DB
  wo *lv.WriteOptions
  ro *lv.ReadOptions
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

func newDB() *dbConn {
  var dbc dbConn

  opts := lv.NewOptions()
  opts.SetCache(lv.NewLRUCache(40*1024*1024))
  opts.SetBlockSize(16*1024)
  opts.SetCreateIfMissing(true)

  var e error
  dbc.db, e = lv.Open(fp.Join("ldb", "user"), opts)
  if e != nil {
    log.Printf("%v", e)
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

  v, _ := dbc.db.Get(dbc.ro, []byte("igorsd"))
  log.Printf("%v", v)

  dbc.db.Put(dbc.wo, []byte("igord"), val)
  log.Printf("Ura!")

  return &dbc
}
