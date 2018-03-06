#!/bin/bash
#
# GNU C/C++ environment variables
#
########################################################################################

case $OSTYPE in
    darwin*)
        # Override XCode4/gcc tendency to use arch=PPC when building libraries
        export ARCHFLAGS="-arch x86_64"

        # Default compiler flags that are safe to use with python and numpy extensions
        export CFLAGS="-arch i386 -arch x86_64"
        export FFLAGS="-m32 -m64 -ff2c"
        export LDFLAGS="-Wall -undefined dynamic_lookup -bundle -arch i386 -arch x86_64"
        export CC="gcc"
        export CXX="g++ -arch i386 -arch x86_64"

        # Aliases for gcc version selection via MacPorts
        alias gcc_list="sudo port select --list gcc"
        alias gcc_select="sudo port select --set gcc"

        # Set pkgconfig search path
        export LOCAL_PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig:$HOME/.local/lib64/pkgconfig:
        export PKG_CONFIG_PATH=$LOCAL_PKG_CONFIG_PATH:/opt/local/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig
        ;;

    linux*)
        # Build only x86_64 architecture
        export CFLAGS="-v -Wall -m64 -fPIC"
        export CC="gcc"
        export CXX="g++"

        # Set pkgconfig search path
        export LOCAL_PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig:$HOME/.local/lib64/pkgconfig:
        export PKG_CONFIG_PATH=$LOCAL_PKG_CONFIG_PATH:/usr/local/lib/pkgconfig:/usr/lib64/pkgconfig:/usr/share/pkgconfig
        ;;
esac
