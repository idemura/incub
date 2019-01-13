#pragma once

#include <cstring>
#include <glog/logging.h>

#define LOGF LOG(FATAL)
#define LOGE LOG(ERROR)
#define LOGW LOG(WARNING)
#define LOGI LOG(INFO)
#define LOGD LOG(DEBUG)

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

// To init gtest: testing::InitGoogleTest(&argc, argv);
// To init gmock: testing::InitGoogleMock(&argc, argv);
