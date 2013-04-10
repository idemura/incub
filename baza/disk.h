#ifndef DISK_H_
#define DISK_H_

#include "defs.h"

struct disk_file {
    int (*write) (struct disk_file *file, const void *buf,
        uofs buf_size, uofs *written);

    int (*read) (struct disk_file *file, void *buf, uofs buf_size, uofs *read);
    int (*seek) (struct disk_file *file, uofs offset);
    int (*get_offset) (struct disk_file *file, uofs *offset);
};

#endif
