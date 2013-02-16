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

func testMath() {
  const K = 4

  pow10 := func (n int) int {
    p := 1
    for i := 0; i < n; i++ {
      p *= 10
    }
    return p
  }

  check := func (n int) bool {
    n = n * n;
    d1 := n % 10;
    n /= 10;
    d2 := n % 10;
    if d1 != 5 && d2 != 5 {
      return false
    }
    for i := 2; i < K; i++ {
      n /= 10
      if n % 10 != 5 {
        return false
      }
    }
    return true
  }

  ub := pow10(K)
  for i := 0; i < ub; i++ {
    if check(i) {
      fmt.Printf("%v -> %v\n", i, i * i);
    }
  }
}

func main() {
  // testMarshal()
  // testMongoDB()
  // testUrlGet()
  // testTemplate()
  testMath()
}
