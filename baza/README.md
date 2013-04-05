# Overview

Baza open-source key-value store.

## Requirements

- C99 compiler

## Build and test

Build release and run tests:

    make && make test
    ./bin/opt/baza_test

Debug mode:

    make main cfg=debug && make test cfg=debug
    ./bin/debug/baza_test

## Contributors

- Igor Demura

## License

Copyright 2013 Igor Demura

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0
