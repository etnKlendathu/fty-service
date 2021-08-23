cmake_minimum_required(VERSION 3.13)
cmake_policy(VERSION 3.13)

project(fty-service
    VERSION 1.0.0
    DESCRIPTION "Service test"
)

##############################################################################################################
find_package(fty-cmake PATHS ${CMAKE_BINARY_DIR}/fty-cmake)
##############################################################################################################


##############################################################################################################
etn_target(exe ${PROJECT_NAME}
    SOURCES
        main.cpp
        services.h
    USES
        fty-pack
        pthread
        fty-utils
        fty_common_messagebus
        mlm
        czmq
)
##############################################################################################################
