#ifndef DISK_H_
#define DISK_H_

#include "defs.h"

#define IO_OK     0
#define IO_ERROR  -1

#define MODE_READ       0x01
#define MODE_WRITE      0x02
#define MODE_CREATE     0x04
#define MODE_TRUNC      0x08

typedef void *file_t;

struct disk_io {
    file_t (*open) (const char *name, int mode);
    void (*close) (file_t f);
    int  (*write) (file_t f, const void *buf,
            uofs buf_size, uofs *bytes_written);
    int  (*read) (file_t f, void *buf, uofs buf_size, uofs *bytes_read);
    int  (*seek) (file_t f, uofs offset);
    int  (*get_offset) (file_t f, uofs *offset);
};

struct disk_io *get_disk_io();
struct disk_io *get_memory_file_io();

#endif
