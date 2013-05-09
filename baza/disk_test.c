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
#include "disk.c"
#include "test.h"

void disk_test()
{
    test_begin("Disk");
    struct disk_io *io = get_disk_io();
    TEST_CHECKM(io != 0, "I/O interface is NULL");
    file_t f = io->open("1.tmp", MODE_READ | MODE_WRITE | MODE_CREATE |
                                 MODE_TRUNC);
    TEST_CHECKM(f != 0, "File not opened");
    char buf[] = "Hello file\n";
    uofs committed = 0;
    io->write(f, buf, sizeof(buf), &committed);
    io->close(f);
    test_end();
}
