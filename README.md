#Overview

TapeColl open-source project.

##Requirements

- Go1.0+
- github.com/gorilla/sessions

##Install

Third party packages:
  go get -u github.com/gorilla/sessions

Build leveldb and get levigo:
  From web/leveldb-1.7.0/ and run:
    make
  From web/ run:
    ./get-levigo

Now you are ready to build:
  ./build

##Contributors

- Igor Demura

##License

Copyright (c) Igor Demura 2012
Released under the GPL v3 license.
