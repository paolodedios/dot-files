#!/bin/bash
#
# Exported Shell Variables
# ------------------------
#
# @author paolodedios
#
########################################################################################


########################################################################################
# Declare platform specific applications
########################################################################################

function set_local_dev_environment_vars()
{
    # Declare personal projects directory
    if [ -d $LOCAL_PROJECTS ]; then
        export PROJECT_HOME=$LOCAL_PROJECTS
    fi

    # Declare platform agnostic applications
    if [ -d $LOCAL_APP_HOME ]; then

        # Declare C toolchain [ Updated 01/10/2014 ]
        #
        # XCode     : https://developer.apple.com/xcode/
        #
        export CC=gcc
        export CXX=g++
        export FFLAGS="-ff2c"

        # Declare platform agnostic applications [ Updated 12/04/2014 ]
        #
        # JDK       : http://www.oracle.com/technetwork/java/javase/downloads/index.html
        #
        export JDK_VERSION=1.8.0

        # Set JAVA_HOME to JDK 8
        export JAVA_HOME=$(/usr/libexec/java_home -v $JDK_VERSION)

        # Set GRADLE_HOME
        export GRADLE_HOME=/opt/local/share/java/gradle
    fi

    # Add shared data and log folder to the path, if you have it.
    if [ -d $LOCAL_APP_HOME/var ]; then
        # Add work directory for Packer.
        #
        # When debugging the packer build process, set the
        # environment variable PACKER_LOG=1. The log output
        # will be displayed via stdout but can be optionally
        # redirected to a file via PACKER_LOG_PATH
        #
        # http://www.packer.io/docs/other/debugging.html
        #
        export PACKER_CACHE_DIR=$LOCAL_DATA_PATH/packer/cache
        export PACKER_BUILD_DIR=$LOCAL_DATA_PATH/packer/build
    fi
}

function set_python_environment_vars()
{
    # Python virtual environment
    if [ $(type -p virtualenvwrapper.sh) ]; then

        # Set default virtualenv python version
        export VIRTUALENV_PYTHON_PATH="/opt/local/bin"
        export VIRTUALENV_PYTHON_VERSION="3.6"

        # Set virtualenv working directory
        export WORKON_HOME=$LOCAL_APP_HOME/bin/python/virtualenvs

        # Make pip use the same directory for virtualenvs as virtualenvwrapper
        export PIP_VIRTUALENV_BASE=$WORKON_HOME

        # Ensure that pip only runs if there is a virtualenv currently activated
        export PIP_REQUIRE_VIRTUALENV=true

        # Makes pip detect an active virtualenv and install to it, without
        # having to pass it the -E parameter
        export PIP_RESPECT_VIRTUALENV=true

        # Ensure that the MacPorts versions of Python and virtualenv are used
        # for all virtualenv sessions.
        export VIRTUALENVWRAPPER_PYTHON=$VIRTUALENV_PYTHON_PATH/python
        export VIRTUALENVWRAPPER_VIRTUALENV=$VIRTUALENV_PYTHON_PATH/virtualenv

        # Ensure that all new environments are isolated from the system
        # site-packages directory by passing "no-site-packages" as the default
        # argument for virtualenv
        export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'

        # Load Python virtualenv wrapper functions
        source virtualenvwrapper.sh > /dev/null 2>&1
    fi

    # NLTK data location used by the command
    #
    #    $ python -m nltk.downloader all
    #
    # to install corpora, models, and tokenizers
    #
    # @see http://www.nltk.org/data.html for details
    export NLTK_DATA=$HOME/.bin.local/var/data/nltk


    # SpaCy data location used by the command
    #
    #    $ python -m spacy.en.download all --force
    #
    export SPACY_DATA=$HOME/.bin.local/var/data/spacy
}

function set_c_environment_vars()
{
    if [ "$OS" = "darwin" ]; then
        # Override XCode4/gcc tendency to use arch=PPC when building libraries
        export ARCHFLAGS="-arch x86_64"

        # Default compiler flags that are safe to use with python and numpy extensions
        export CFLAGS="-arch i386 -arch x86_64"
        export FFLAGS="-m32 -m64"
        export LDFLAGS="-Wall -undefined dynamic_lookup -bundle -arch i386 -arch x86_64"
        export CC="gcc"
        export CXX="g++ -arch i386 -arch x86_64"
    else
        # Build only x86_64 architecture
        export CFLAGS="-v -Wall -m64"
        export CC="gcc"
        export CXX="g++"
    fi

}

function set_cuda_environment_vars()
{
    # The CUDA toolkit download will install the CUDA driver, CUDA toolkit
    # supplements and CUDA samples.
    #
    # CUDA Driver: This will install /Library/Frameworks/CUDA.framework and the
    # UNIX-compatibility stub /usr/local/cuda/lib/libcuda.dylib that refers to
    # it.
    #
    # CUDA Toolkit: The CUDA Toolkit supplements the CUDA Driver with compilers
    # and additional libraries and header files that are installed into
    # /Developer/NVIDIA/CUDA-6.5 by default. Symlinks are created in
    # /usr/local/cuda/ pointing to their respective files in
    # /Developer/NVIDIA/CUDA-6.5/. Previous installations of the toolkit will be
    # moved to /Developer/NVIDIA/CUDA-#.# to better support side-by-side
    # installations.
    #
    # CUDA Samples (read-only): A read-only copy of the CUDA Samples is
    # installed in /Developer/NVIDIA/CUDA-6.5/samples. Previous installations of
    # the samples will be moved to /Developer/NVIDIA/CUDA-#.#/samples to better
    # support side-by-side installations.
    #
    # @see http://docs.nvidia.com/cuda/index.html
    #
    if [ "$OS" = "darwin" ]; then
        #
        # The macOS CUDA installer places the library in /Developer/NVIDIA/CUDA-X.X.
        # The following path, /opt/local/cuda should symlink to the real path in
        # /Developer.
        #
        export LOCAL_CUDA_HOME=/opt/local/cuda
        export LOCAL_CUDA_PATH=$LOCAL_CUDA_HOME/bin
        export LOCAL_CUDA_LIBRARY_PATH=$LOCAL_CUDA_HOME/lib

        if [ -d $LOCAL_CUDA_PATH -a -d $LOCAL_CUDA_LIBRARY_PATH ]; then
            export PATH=$LOCAL_CUDA_PATH:$PATH
            export DYLD_FALLBACK_LIBRARY_PATH=$LOCAL_CUDA_LIBRARY_PATH:$DYLD_FALLBACK_LIBRARY_PATH
        fi

    else
        export LOCAL_CUDA_HOME=/usr/local/cuda
        export LOCAL_CUDA_PATH=$LOCAL_CUDA_HOME/bin
        export LOCAL_CUDA_LIBRARY_PATH=$LOCAL_CUDA_HOME/lib64

        if [ -d $LOCAL_CUDA_PATH -a -d $LOCAL_CUDA_LIBRARY_PATH ]; then
            export PATH=$LOCAL_CUDA_PATH:$PATH
            export LD_LIBRARY_PATH=$LOCAL_CUDA_LIBRARY_PATH:$LD_LIBRARY_PATH
        fi
    fi
}

function set_go_lang_environment_vars()
{
    # Declare personal projects directory
    if [ -d $LOCAL_PROJECTS ]; then
        export GO_PROJECT_HOME=$LOCAL_PROJECTS
    else
        export GO_PROJECT_HOME=$HOME
    fi

    # Set the top level GOPATH environment variable
    # @see https://golang.org/doc/code.html#Workspaces
    #
    export GOPATH=$GO_PROJECT_HOME/Go
    export PATH=$GOPATH/bin:$PATH
}


function set_nodejs_nave_environment_vars()
{
    # Set virtualenv working directory, defaults to $HOME/.nave otherwise
    export NAVE_DIR=$LOCAL_APP_HOME/bin/nodejs/virtualenvs
}

########################################################################################
# Export OS indepenedent environment variables
########################################################################################

#
# OS independent environment variables should be set after platform specific
# variables have already been defined and abstracted away
#

# Set local dev environment variables
#
# Local dev environment variables have precedence over shared and system
# environment variables
set_local_dev_environment_vars

# Set local C/C++ build environment variables
#
# Local programming environment variables are set after shared and local
# environment dependencies are set
set_c_environment_vars

# Set local python virtualenv variables
#
# Local programming environment variables are set after shared and local
# environment dependencies are set
set_python_environment_vars

# Set CUDA, OpenCL and other toolkit environment variables
#
# Toolkits that may depend on shared or local environment variables go last
set_cuda_environment_vars

# Set local Go language environment variables
set_go_lang_environment_vars

# Set local NodeJS/nave environment variables
set_nodejs_nave_environment_vars
