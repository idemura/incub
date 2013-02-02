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

func Init(url string) {
  if !Open(url) {
    return
  }

  defer Close()

  ctx := NewDataCtx()
  ctx.users.DropCollection()
  ctx.users.EnsureIndex(mgo.Index{
    Key: []string{"FirstName"},
    Background: false,
    Sparse: true,
  })
  ctx.users.EnsureIndex(mgo.Index{
    Key: []string{"LastName"},
    Background: false,
    Sparse: true,
  })
  ctx.users.EnsureIndex(mgo.Index{
    Key: []string{"UserName"},
    Background: false,
    Unique: true,
    DropDups: true,
    Sparse: true,
  })
  ctx.users.EnsureIndex(mgo.Index{
    Key: []string{"Email"},
    Background: false,
    Unique: true,
    DropDups: true,
    Sparse: true,
  })
  ctx.NewUser(&User{
    bson.NewObjectId(),
    "Igor", "Demura",
    "demi",
    "idemura@yandex.ru",
    "sv32x",
  })

  demi := ctx.UserFromEmail("idemura@yandex.ru")
  if demi == nil {
    log.Printf("DB ERROR: Can't find user demi")
  }
  
  log.Printf("DB init done")
}
