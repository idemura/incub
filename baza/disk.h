#ifndef DISK_H_
#define DISK_H_

#include "defs.h"

#define IO_OK   0

#define MODE_READ   1
#define MODE_WRITE  2

// File contains data like handle and etc. and pointer to function set working
// with this file.
struct file;

struct disk_io {
    struct file *(*open) (const char *name, int mode);
    void (*close) (struct file *f);
    int  (*write) (struct file *f, const void *buf,
            uofs buf_size, uofs *written);
    int  (*read) (struct file *f, void *buf, uofs buf_size, uofs *read);
    int  (*seek) (struct file *f, uofs offset);
    int  (*get_offset) (struct file *f, uofs *offset);
};

// void set_disk_io(struct disk_io *io);
// struct file *disk_open(const char *name, int mode);
// void disk_close(struct file *f);
// int  disk_write(struct file *f, const void *buf, uofs buf_size, uofs *written);
// int  disk_read(struct file *f, void *buf, uofs buf_size, uofs *read);
// int  disk_seek(struct file *f, uofs offset);
// int  disk_get_offset(struct file *f, uofs *offset);

struct disk_io *get_disk_io();

#endif
