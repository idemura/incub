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
  "time"
  "labix.org/v2/mgo"
  "labix.org/v2/mgo/bson"
)

type User struct {
  id bson.ObjectId
  FirstName string
  LastName string
  UserName string
  Email string
  Password string
}

func (obj *User) marshal() bson.M {
  return bson.M{
    "_id": obj.id,
    "FirstName": obj.FirstName,
    "LastName": obj.LastName,
    "UserName": obj.UserName,
    "Email": obj.Email,
    "Password": obj.Password,
  }
}

type Post struct {
  id bson.ObjectId
  owner *User
  Time time.Time
  Text string
}

func (obj *Post) marshal() bson.M {
  return bson.M{
    "_id": obj.id,
    "OwnerId": obj.owner.id,
    "Time": obj.Time,
    "Text": obj.Text,
  }
}

type Context struct {
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

func NewContext() *Context {
  if dbs == nil {
    return nil
  }
  db := dbs.DB("tapecoll")
  return &Context{
    db,
    db.C("Users"),
    db.C("Posts"),
  }
}

func (ctx *Context) UserFromEmail(email string) *User {
  var proto bson.M
  e := ctx.users.Find(bson.M{"Email": email}).One(&proto)
  if e != nil {
    log.Printf("DB ERROR find: %v", e)
    return nil
  }
  return &User{
    id: proto["_id"].(bson.ObjectId),
    FirstName: proto["FirstName"].(string),
    LastName: proto["LastName"].(string),
    UserName: proto["UserName"].(string),
    Email: proto["Email"].(string),
    Password: proto["Password"].(string),
  }
}

func (ctx *Context) NewUser(user *User) error {
  e := ctx.users.Insert(user.marshal())
  if e != nil {
    log.Printf("DB ERROR NewUser: %v", e)
  }
  return e
}

func (ctx *Context) NewPost(post *Post) error {
  e := ctx.posts.Insert(post.marshal())
  if e != nil {
    log.Printf("DB ERROR NewPost: %v", e)
  }
  return e
}

func (ctx *Context) GetUserPosts(user *User) ([]*Post, error) {
  var res []bson.M
  e := ctx.posts.Find(bson.M{"OwnerId": user.id}).All(&res)
  if e != nil {
    log.Printf("DB ERROR: %v", e)
    return nil, e
  }
  posts := make([]*Post, len(res))
  for i, proto := range res {
    p := &Post{
      id: proto["_id"].(bson.ObjectId),
      owner: user,
      Time: proto["Time"].(time.Time),
      Text: proto["Text"].(string),
    }
    posts[i] = p
  }
  return posts, nil
}
