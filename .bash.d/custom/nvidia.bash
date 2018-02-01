#!/bin/bash
#
# NVIDIA libraries and tools environment variables
#
########################################################################################

case $OSTYPE in
    darwin*)
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
        ;;

    linux*)
        export CUDA_HOME=/usr/local/cuda
        export PATH=$CUDA_HOME/bin:$PATH
        export MANPATH=$CUDA_HOME/man:${MANPATH}

        function install_hardware_driver()
        {
            # Drivers available via https://www.nvidia.com/object/unix.html
            NVIDIA_HDWR_DRIVER_VERSION=$1
            NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE="NVIDIA-Linux-x86_64-${NVIDIA_HDWR_DRIVER_VERSION}.run"
            NVIDIA_HDWR_DRIVER_DOWNLOAD_PATH="${NVIDIA_HDWR_DRIVER_VERSION}/${NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE}"
            NVIDIA_HDWR_DRIVER_DOWNLOAD_URL="http://us.download.nvidia.com/XFree86/Linux-x86_64/${NVIDIA_HDWR_DRIVER_DOWNLOAD_PATH}"

            echo "Downloading NVIDIA hardware driver [ver ${NVIDIA_HDWR_DRIVER_VERSION}] from us.download.nvidia.com"
            pushd .
            cd /tmp

            wget --no-check-certificate $NVIDIA_HDWR_DRIVER_DOWNLOAD_URL

            # Install NVIDIA driver
            if [ -f $NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE ]; then
                echo "Installing NVIDIA hardware driver ${NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE}"
                bash $NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE \
                     --accept-license                       \
                     --no-questions                         \
                     --ui=none                              \
                     --silent                               \
                     --dkms                                 \
                     --disable-nouveau                      \
                     --no-glvnd-glx-client                  \
                     --no-install-libglvnd
            else
                echo "ERROR: Hardware driver download failed!"
                exit 1
            fi

            rm -f $NVIDIA_HDWR_DRIVER_INSTALLER_RUN_FILE
            popd
        }

        #
        # Declare the same LOCAL_CUDA paths as macOS to ensure some level of
        # script portability
        #
        export LOCAL_CUDA_HOME=$CUDA_HOME
        export LOCAL_CUDA_PATH=$LOCAL_CUDA_HOME/bin
        export LOCAL_CUDA_LIBRARY_PATH=$LOCAL_CUDA_HOME/lib64

        if [ -d $LOCAL_CUDA_PATH -a -d $LOCAL_CUDA_LIBRARY_PATH ]; then
            export PATH=$LOCAL_CUDA_PATH:$PATH
            export LD_LIBRARY_PATH=$LOCAL_CUDA_LIBRARY_PATH:$LD_LIBRARY_PATH
        fi

        ;;
esac
