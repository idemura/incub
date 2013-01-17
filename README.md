# Overview

TapeColl open-source project.

## Requirements

- Go 1.0+
- [Gorilla sessions](https://github.com/gorilla/sessions)
- [Levigo](https://github.com/jmhodges/levigo)

## Init

First, build LevelDB and then install levigo with script:

    cd leveldb-1.7.0
    make
    cd ..
    ./get-levigo

Install other packages:

    go get -u github.com/gorilla/sessions

## Build

Build project with script:
    
    ./build

## Contributors

- Igor Demura

## License

Copyright (c) Igor Demura 2012

GPL v3 license
