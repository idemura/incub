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
  Id bson.ObjectId "_id"
  FirstName string "FirstName"
  LastName string "LastName"
  UserName string "UserName"
  Email string "Email"
  Password string "Password"
}

type Post struct {
  Id bson.ObjectId "_id"
  OwnerId bson.ObjectId "OwnerId"
  Text string "Text"
  Date string "Date"
}

type DataCtx struct {
  db *mgo.Database
  users *mgo.Collection
  posts *mgo.Collection
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

func NewDataCtx() *DataCtx {
  if dbs == nil {
    return nil
  }
  db := dbs.DB("tapecoll")
  return &DataCtx{
      db,
      db.C("Users"),
      db.C("Posts"),
    }
}

func (ctx *DataCtx) UserFromEmail(email string) *User {
  var user User
  e := ctx.users.Find(bson.M{"Email": email}).One(&user)
  if e != nil {
    log.Printf("DB ERROR find: %v", e)
    return nil
  }
  return &user
}

func (ctx *DataCtx) NewUser(user *User) bool {
  e := ctx.users.Insert(user)
  if e != nil {
    log.Printf("DB ERROR insert: %v", e)
  }
  return e != nil
}

func (ctx *DataCtx) NewPost(user *User) bool {
  return false
}
