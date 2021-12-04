#################################################################################
### MODE: determines what compiler flags to use
### SYSTEM: determines the intermediate and final file directory names (for now)
### DEFINES:       list of predefined macros to pass to the compiler
### INC_DIRS:      list of include paths to pass to the compiler
### DEP_INC_DIRS:  list of include paths to search for dependencies in
### LIB_DIRS:      list of library paths to pass to the linker
### TARGET_TYPE:   determines whether to build executables or libraries
### SOURCES:       list of source (c++) files
### SOURCE_LIBS:   list of locally built libs (they are included as a dependency)
### LINK_LIBS:     list of libraries to link againsg on top of SOURCE_LIBS
### TARGETS:       list of target final targets (library names or executable files)

ifndef MODE
#MODE=release
MODE=debug
endif

PROJECT_MAKEFILE=

include ../common.mak

#DEFINES = SIM_TURBO

TARGET_TYPE = LIBRARY

SOURCES =
SOURCES += config_file.cpp
SOURCES += curses_win.cpp
SOURCES += console.cpp
SOURCES += sim_logger.cpp
SOURCES += logger.cpp
SOURCES += utils.cpp
#SOURCES += ui.cpp
ifeq ("$(SYSTEM)","mingw")
SOURCES += vtap_win32.cpp
endif
ifeq ("$(SYSTEM)","cygwin")
SOURCES += vtap_win32.cpp
endif
ifeq ("$(SYSTEM)","linux")
SOURCES += vtap_linux.cpp
endif



SOURCE_LIBS =

LINK_LIBS =

TARGETS = 
TARGETS += sim_lib

include ../engine.mak
