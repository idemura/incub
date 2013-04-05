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

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at \
  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
