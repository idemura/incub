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
static struct disk_io smemory_file_io;

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
        flags |= O_CREAT;
    }
    if (mode & MODE_TRUNC) {
        flags |= O_TRUNC;
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

static int disk_write(file_t f, const void *buf, uofs buf_size,
    uofs *bytes_written)
{
    return IO_OK;
}

static int disk_read(file_t f, void *buf, uofs buf_size, uofs *bytes_read)
{
    struct disk_file *file = f;
    ssize_t res = read(file->fd, buf, buf_size);
    if (res == -1) {
        *bytes_read = 0;
        return IO_ERROR;
    } else {
        *bytes_read = (uofs)res;
        return IO_OK;
    }
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

struct disk_io *get_memory_file_io()
{
    // if (!smemory_file_io.open) {
    //     smemory_file_io.open = mem_open;
    //     smemory_file_io.close = mem_close;
    //     smemory_file_io.write = mem_write;
    //     smemory_file_io.read = mem_read;
    //     smemory_file_io.seek = mem_seek;
    //     smemory_file_io.get_offset = mem_get_offset;
    // }
    return &sdisk_io;
}
