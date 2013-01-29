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
package data

import (
  "log"
  "labix.org/v2/mgo"
  "labix.org/v2/mgo/bson"
)

type User struct {
  Id []byte "_id"
  FirstName string "FirstName"
  LastName string "LastName"
  UserName string "UserName"
  Email string "Email"
  Password string "Password"
}

type DataCtx struct {
  db *mgo.Database
  user *mgo.Collection
}

var dbs *mgo.Session

func Open(url string) bool {
  s, e := mgo.Dial(url)
  if e != nil {
    log.Printf("DB ERROR: %v", e)
    return false
  }
  dbs = s
  return true
}

func Close() {
  if dbs != nil {
    dbs.Close()
    dbs = nil
  }
}

func Init(url string) {
  if !Open(url) {
    return
  }

  defer Close()

  ctx := NewDataCtx()
  ctx.user.DropCollection()
  ctx.user.EnsureIndex(mgo.Index{
    Key: []string{"FirstName"},
    Background: false,
    Sparse: true,
  })
  ctx.user.EnsureIndex(mgo.Index{
    Key: []string{"LastName"},
    Background: false,
    Sparse: true,
  })
  ctx.user.EnsureIndex(mgo.Index{
    Key: []string{"UserName"},
    Background: false,
    Unique: true,
    DropDups: true,
    Sparse: true,
  })
  ctx.user.EnsureIndex(mgo.Index{
    Key: []string{"Email"},
    Background: false,
    Unique: true,
    DropDups: true,
    Sparse: true,
  })
  ctx.NewUser(&User{
    nil,
    "Igor", "Demura",
    "demi",
    "idemura@yandex.ru",
    "sv32x",
  })

  log.Printf("DB init done")
}

func NewDataCtx() *DataCtx {
  if dbs == nil {
    return nil
  }
  db := dbs.DB("tapecoll")
  return &DataCtx{
      db,
      db.C("User"),
    }
}

func (ctx *DataCtx) UserFromEmail(email string) *User {
  var user User
  e := ctx.user.Find(bson.M{"Email": email}).One(&user)
  if e != nil {
    return nil
  }
  return &user
}

func (ctx *DataCtx) NewUser(user *User) bool {
  e := ctx.user.Insert(user)
  return e != nil
}
