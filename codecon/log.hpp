#pragma once

#include <cstring>
#include <glog/logging.h>

inline void initLog(int argc, char **argv) {
    google::InitGoogleLogging(argv[0]);
    google::LogToStderr();
    for (int i = 1; i < argc; i++) {
        if (std::strcmp(argv[i], "-v") == 0) {
            FLAGS_v = 1;
            break;
        }
    }
}
