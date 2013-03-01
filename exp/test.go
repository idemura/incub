package main

import (
  "fmt"
  "os"
  fp "path/filepath"
  "net/http"
  "io/ioutil"
  tt "html/template"
  // "labix.org/v2/mgo"
  // "labix.org/v2/mgo/bson"
)

func testPath() {
  const pat = "?ur*.hs"
  visit := func (path string, fi os.FileInfo, err error) error {
    if m, _ := fp.Match(pat, path); m {
      fmt.Printf("%s\n", path)
    }
    return nil
  }

  fp.Walk(".", visit)
}

func testUrlGet() {
  r, e := http.Get("http://google.com")
  if e != nil {
    fmt.Printf("ERROR: %v\n", e)
    return
  }
  c, e := ioutil.ReadAll(r.Body)
  r.Body.Close()
  if e != nil {
    fmt.Printf("ERROR: %v\n", e)
    return
  }
  fmt.Printf("%v\n", string(c))
}

type TplCtx struct {
  UserEmail tt.JS
}

const tpl = `<!DOCTYPE html>
<html>
<head>
<title>Test</title>
</head>
<body>
<script>
  // Here email goes
  var currentUser = {{.UserEmail}};
</script>
</body>
</html>
`

func testTemplate() {
  // const tpl_file = "../web/templates/test.html"
  // t, e := tt.ParseFiles(tpl_file)
  t, e := tt.New("").Parse(tpl)
  if e != nil {
    fmt.Printf("ERROR!\n")
    return
  }
  t.Execute(os.Stdout, &TplCtx{"null"})
}

type User struct {
  Name string
  EmailAddr string "Email"
}

// func testMarshal() {
//   email := "idemura@mail.ru"
//   user := User{"Igor", email}
//   bs, e := bson.Marshal(&user)
//   if e != nil {
//     fmt.Printf("ERROR: %v\n", e)
//     return
//   }
//   fmt.Printf("SUCCESS:\n")
//   fmt.Printf("  %v\n", len(bs))
//   fmt.Printf("  %v\n", string(bs))
// }

// func testMongoDB() {
//   email := "idemura@mail.ru"
//   user := User{"Igor", email}
//
//   session, e := mgo.Dial("localhost")
//   if e != nil {
//     fmt.Printf("ERROR: Dial %v\n", e)
//     return
//   }
//   defer session.Close()
//   db := session.DB("mytest")
//   coll := db.C("User")
//   e = coll.Insert(&user)
//   if e != nil {
//     fmt.Printf("ERROR: Insert %v\n")
//     return
//   }
//   var userOut User
//   e = coll.Find(bson.M{"Email": email}).One(&userOut)
//   if e != nil {
//     fmt.Printf("ERROR: Find %v\n", e)
//     return
//   }
//   fmt.Printf("SUCCESS: %v\n", userOut)
// }

func gcd(a, b int64) int64 {
  if (a < b) {
    a, b = b, a
  }
  for r := a % b; r != 0; r = a % b {
    a, b = b, r
  }
  return b
}

func testMath() {
  var P int64 = 12
  var Q int64 = 5
  for i := int64(1); i < 2000; i++ {
    s := i * (i + 1) + P
    t := i + Q
    d1 := gcd(s, t)
    d2 := gcd(i + Q, P + Q * (Q - 1))
    if d1 != d2 {
      fmt.Printf("%d - (%d, %d) = %d != %d\n", i, s, t, d1, d2)
    } else {
      // fmt.Printf("%d - OK\n", i)
    }
  }
  // for i := int64(1); i < 2000; i += 2 {
  //   i_2 := i * i;
  //   i_4 := i_2 * i_2;
  //   s := i_4 + 4 * i_2 + 12
  //   t := i * (i_4 + 5 * i_2 + 5)
  //   d1 := gcd(s, t)
  //   d2 := gcd()
  //   if d != 1 {
  //     fmt.Printf("%d - (%d, %d) = %d\n", i, s, t, d)
  //   }
  // }
}

func main() {
  // testMarshal()
  // testMongoDB()
  // testUrlGet()
  // testTemplate()
  // testMath()
}
