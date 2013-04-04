#include "defs.h"
#include <stdio.h>

/*
TODO:
-----
Use stack and refactor splitting.
Make compare function in BTree.
Binary search in BTree node.
Mem counter: atomic add/sub

Add allocator for btree nodes, because it allocs same block size always,
possible optimizations.

Make linked list on leaves (bottom level nodes).

Multithreaded btree. Smart lock of nodes. Have no idea how to do this.

Btree disk operations. I/O queue. File chunks allocator.

Command and interpreter. Do I need VM?

HTTP server. This will just report something. Main thing is to listen for
commands.
*/

int main()
{
    printf("main\n");
    return 0;
}
