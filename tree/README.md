## Overview
Tree utility in Clojure. Renders a file tree in the terminal.

Usage:
```tree <directory> [file filter regex]```

Sample rendering:
```
user@machine:tree$ tree .
.
├─ .gitignore
├─ .lein-repl-history
├─ README.md
├─ project.clj
├─ src
│  └─ tree
│     └─ main.clj
└─ target
   ├─ classes
   └─ stale
      └─ dependencies
```

## Build and install

Assuming your target directory for binaries is `~/bin`, you can build and
install uberjar with commands:
```
$ lein with-profile opt uberjar
$ cp target/opt+uberjar/tree-0.1.0-standalone.jar ~/bin
$ cp tree ~/bin
```

## License

Copyright © 2013 Igor Demura. Distributed under the Eclipse Public License.
