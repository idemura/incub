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

static uofs s_block = 4096;

// struct disk_file_raw {
//     struct disk_file file;
//     int fd;
// };

// struct disk_file *open_file(const char *name)
// {
//     struct disk_file_raw *obj = mem_alloc(sizeof(*obj));
//     obj->fd = open(name);
//     return &obj->file;
// }

void disk_setblocksize(uofs block)
{
    s_block = block;
}

uofs disk_getblocksize()
{
    return s_block;
}
