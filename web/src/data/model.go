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
  firstName string
  lastName string
  userName string
  email string
  password string
}

func (obj *User) marshal() bson.M {
  return bson.M{
    "_id": obj.id,
    "FirstName": obj.firstName,
    "LastName": obj.lastName,
    "UserName": obj.userName,
    "Email": obj.email,
    "Password": obj.password,
  }
}

func NewUser(firstName, lastName, userName, email, password string) *User {
  return &User{
    id: bson.NewObjectId(),
    firstName: firstName,
    lastName: lastName,
    userName: userName,
    email: email,
    password: password,
  }
}

func NewBareUser(email string) *User {
  return &User{
    email: email,
  }
}

func (obj *User) FirstName() string {
  return obj.firstName
}

func (obj *User) LastName() string {
  return obj.lastName
}

func (obj *User) UserName() string {
  return obj.userName
}

func (obj *User) Email() string {
  return obj.email
}

func (obj *User) Password() string {
  return obj.password
}

type Post struct {
  id bson.ObjectId
  owner *User
  time time.Time
  text string
}

type PostList []*Post

func (obj *Post) marshal() bson.M {
  return bson.M{
    "_id": obj.id,
    "OwnerId": obj.owner.id,
    "Time": obj.time,
    "Text": obj.text,
  }
}

func NewPost(owner *User, time time.Time, text string) *Post {
  return &Post{
    id: bson.NewObjectId(),
    owner: owner,
    time: time,
    text: text,
  }
}

func (obj *Post) Owner() *User {
  return obj.owner
}

func (obj *Post) Time() time.Time {
  return obj.time
}

func (obj *Post) Text() string {
  return obj.text
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
    firstName: proto["FirstName"].(string),
    lastName: proto["LastName"].(string),
    userName: proto["UserName"].(string),
    email: proto["Email"].(string),
    password: proto["Password"].(string),
  }
}

func (ctx *Context) SaveUser(user *User) error {
  e := ctx.users.Insert(user.marshal())
  if e != nil {
    log.Printf("DB ERROR SaveUser: %v", e)
  }
  return e
}

func (ctx *Context) SavePost(post *Post) error {
  e := ctx.posts.Insert(post.marshal())
  if e != nil {
    log.Printf("DB ERROR SavePost: %v", e)
  }
  return e
}

func (ctx *Context) GetUserPosts(user *User) PostList {
  it := ctx.posts.Find(bson.M{"OwnerId": user.id}).Iter()
  if it == nil {
    return nil
  }
  ps := make(PostList, 0)
  for i := 0; i < 30; i++ {
    var proto bson.M
    if it.Next(&proto) {
      p := &Post{
        id: proto["_id"].(bson.ObjectId),
        owner: user,
        time: proto["Time"].(time.Time),
        text: proto["Text"].(string),
      }
      ps = append(ps, p)
    } else {
      if e := it.Err(); e != nil {
        log.Printf("DB ERRROR: %v", e)
        return nil
      }
    }
  }
  return ps
}
