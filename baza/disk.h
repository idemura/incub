#ifndef DISK_H_
#define DISK_H_

#include "defs.h"

// File contains data like handle and etc. and pointer to function ser working
// with this file.
struct file;

struct idisk_file {
    int (*write) (struct disk_file *file, const void *buf,
        uofs buf_size, uofs *written);

    int (*read) (struct disk_file *file, void *buf, uofs buf_size, uofs *read);
    int (*seek) (struct disk_file *file, uofs offset);
    int (*get_offset) (struct disk_file *file, uofs *offset);
};

void disk_setblocksize(uofs block);
uofs disk_getblocksize();
struct file *open_file(const char *name, int mode);
void close_file(struct file *f);

#endif

