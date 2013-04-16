/*
  Copyright 2013 Igor Demura

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
#include "disk.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

struct disk_file {
    int fd;
    int mode;
};

static struct disk_io sdisk_io;

static file_t disk_open(const char *name, int mode)
{
    int flags = 0;
    if (mode & MODE_READ) {
        flags |= O_RDONLY;
    }
    if (mode & MODE_WRITE) {
        flags |= O_WRONLY;
    }
    if (mode & MODE_CREATE) {
        mode |= O_CREAT;
    }
    if (mode & MODE_TRUNC) {
        mode |= O_TRUNC;
    }
    int fd = open(name, flags, S_IRUSR | S_IWUSR);
    if (fd < 0) {
        return NULL;
    }

    struct disk_file *file = mem_alloc(sizeof(*file));
    file->fd = fd;
    file->mode = mode;
    return file;
}

static void disk_close(file_t f)
{
    if (!f) {
        return;
    }
    struct disk_file *file = f;
    close(file->fd);
}

static int disk_write(file_t f, const void *buf, uofs buf_size, uofs *written)
{
    return IO_OK;
}

static int disk_read(file_t f, void *buf, uofs buf_size, uofs *read)
{
    return IO_OK;
}

static int disk_seek(file_t f, uofs offset)
{
    return IO_OK;
}

static int disk_get_offset(file_t f, uofs *offset)
{
    return IO_OK;
}

struct disk_io *get_disk_io()
{
    if (!sdisk_io.open) {
        sdisk_io.open = disk_open;
        sdisk_io.close = disk_close;
        sdisk_io.write = disk_write;
        sdisk_io.read = disk_read;
        sdisk_io.seek = disk_seek;
        sdisk_io.get_offset = disk_get_offset;
    }
    return &sdisk_io;
}
