#!/bin/bash
#
# Packer shell environment variables
#
########################################################################################

########################################################################################
#
# Path and variable declarations
#
########################################################################################

# Set PACKER_CONFIG_PATH variable
if [ -d $PACKER_PRIVATE_BUILDER_HOME ]; then
    export PACKER_CONFIG_PATH=$PACKER_PRIVATE_BUILDER_HOME
else
    export PACKER_CONFIG_PATH=/etc/packer
fi

export PACKER_BUILDER_HOME=${PACKER_BUILDER_HOME:-"${PACKER_CONFIG_PATH}"}

# Set PACKER_DATA_PATH variable
if [ -d $LOCAL_APP_HOME/var ]; then
    export PACKER_DATA_PATH=$LOCAL_APP_HOME/var/packer
else
    export PACKER_DATA_PATH=/var/lib/packer
fi


# Declare working directory for Packer.
#
# When debugging the packer build process, set the
# environment variable PACKER_LOG=1. The log output
# will be displayed via stdout but can be optionally
# redirected to a file via PACKER_LOG_PATH
#
# http://www.packer.io/docs/other/debugging.html
#
export PACKER_CACHE_DIR=$PACKER_DATA_PATH/cache
export PACKER_BUILD_DIR=$PACKER_DATA_PATH/build

########################################################################################
#
# Packer build helpers
#
########################################################################################

function ensure_cli_tools()
{
    if [ ! $(type -p packer) ]; then
        error "Missing packer JSON builder. Please install."
        return 1
    fi
}

function build_centos7_vagrant_vmware_vm()
{
    export VAGRANT_INSECURE_KEY=$(eval echo ~/.ssh/vagrant_insecure_key.pem)
    export VAGRANT_SECURE_KEY=$(eval echo ~/.ssh/vagrant_local.pem)

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    PACKER_VAGRANT_VARIABLES="centos-7-x86_64-template-build-config-vagrant.json"
    PACKER_VAGRANT_CONFIG="centos-7-x86_64-minimal-docker-vagrant.json"
    PACKER_VAGRANT_VM_BUILD_TYPE="vmware-iso"
    PACKER_LOG=0

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $PACKER_VAGRANT_CONFIG ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$PACKER_VAGRANT_VARIABLES                   \
                     -var-file=$1                                                                         \
                     -only=$PACKER_VAGRANT_VM_BUILD_TYPE                                                  \
                     $PACKER_VAGRANT_CONFIG
    else
        echo "ERROR. The command 'atl new-vagrant-vm' must be run from the root of the packer config directory."
    fi
}


function build_centos7_vagrant_uefi_vm()
{
    export VAGRANT_INSECURE_KEY=$(eval echo ~/.ssh/vagrant_insecure_key.pem)
    export VAGRANT_SECURE_KEY=$(eval echo ~/.ssh/vagrant_local.pem)

    if [ ! -d $PACKER_CACHE_DIR/qemu/nvram ]; then
        mkdir -p $PACKER_CACHE_DIR/qemu/nvram
    fi

    # Generate the NVRAM storage file name
    export KVM_QEMU_NVRAM_STORAGE_FILE=$PACKER_CACHE_DIR/qemu/nvram/packer-uefi-ovmf-vars-$(date -u +%Y%m%dT%H%M%S).nvram

    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    echo "Using KVM_QEMU_NVRAM_STORAGE_FILE : ${KVM_QEMU_NVRAM_STORAGE_FILE}"
    echo "Using KVM_QEMU_GPU_PCI_ID         : ${KVM_QEMU_GPU_PCI_ID}"
    echo "Using KVM_QEMU_GPU_AUDIO_PCI_ID   : ${KVM_QEMU_GPU_AUDIO_PCI_ID}"

    # Make a copy of the nvram storage base image for packer
    KVM_QEMU_OVMF_VARS_BASE_FILE=/usr/share/edk2/ovmf/OVMF_VARS.fd

    cp -f $KVM_QEMU_OVMF_VARS_BASE_FILE $KVM_QEMU_NVRAM_STORAGE_FILE

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    PACKER_VAGRANT_VARIABLES="centos-7-x86_64-template-build-config-qemu-vagrant.json"

    if [ ! -z "$KVM_QEMU_GPU_PCI_ID" -a ! -z "$KVM_QEMU_GPU_AUDIO_PCI_ID" ]; then
        PACKER_VAGRANT_CONFIG="centos-7-x86_64-minimal-docker-uefi-gpu-vagrant.json"
    else
        PACKER_VAGRANT_CONFIG="centos-7-x86_64-minimal-docker-uefi-vagrant.json"
    fi

    PACKER_VAGRANT_VM_BUILD_TYPE="qemu"
    PACKER_LOG=0

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $PACKER_VAGRANT_CONFIG ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$PACKER_VAGRANT_VARIABLES                   \
                     -var-file=$1                                                                         \
                     -only=$PACKER_VAGRANT_VM_BUILD_TYPE                                                  \
                     $PACKER_VAGRANT_CONFIG
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}


function build_centos7_vagrant_qemu_vm()
{
    export VAGRANT_INSECURE_KEY=$(eval echo ~/.ssh/vagrant_insecure_key.pem)
    export VAGRANT_SECURE_KEY=$(eval echo ~/.ssh/vagrant_local.pem)


    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    PACKER_VAGRANT_VARIABLES="centos-7-x86_64-template-build-config-qemu-vagrant.json"
    PACKER_VAGRANT_CONFIG="centos-7-x86_64-minimal-docker-vagrant.json"

    PACKER_VAGRANT_VM_BUILD_TYPE="qemu"
    PACKER_LOG=0

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $PACKER_VAGRANT_CONFIG ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$PACKER_VAGRANT_VARIABLES                   \
                     -var-file=$1                                                                         \
                     -only=$PACKER_VAGRANT_VM_BUILD_TYPE                                                  \
                     $PACKER_VAGRANT_CONFIG
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}


function build_fedora26_vagrant_qemu_vm()
{
    export VAGRANT_INSECURE_KEY=$(eval echo ~/.ssh/vagrant_insecure_key.pem)
    export VAGRANT_SECURE_KEY=$(eval echo ~/.ssh/vagrant_local.pem)


    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    PACKER_VAGRANT_VARIABLES="fedora-26-x86_64-template-build-config-qemu-vagrant.json"
    PACKER_VAGRANT_CONFIG="fedora-26-x86_64-minimal-docker-vagrant.json"

    PACKER_VAGRANT_VM_BUILD_TYPE="qemu"
    PACKER_LOG=0

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $PACKER_VAGRANT_CONFIG ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$PACKER_VAGRANT_VARIABLES                   \
                     -var-file=$1                                                                         \
                     -only=$PACKER_VAGRANT_VM_BUILD_TYPE                                                  \
                     $PACKER_VAGRANT_CONFIG
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}

########################################################################################
#
# 'packerh' : main entry point for packer helper
#
########################################################################################

function packerh()
{
    if ! ensure_cli_tools; then
        return 1
    fi

    case "$1" in
        new-centos7-vagrant-vmw-vm)
            if [ ! -z $2 ]; then
                build_centos7_vagrant_vmware_vm $2
            else
                echo "ERROR. Vagrant build variables file not specified."
            fi
            ;;
        new-centos7-vagrant-uefi-vm)
            if [ ! -z $2 ]; then
                build_centos7_vagrant_uefi_vm $2
            else
                echo "ERROR. Vagrant build variables file not specified."
            fi
            ;;
        new-centos7-vagrant-qemu-vm)
            if [ ! -z $2 ]; then
                build_centos7_vagrant_qemu_vm $2
            else
                echo "ERROR. Vagrant build variables file not specified."
            fi
            ;;
        new-fedora26-vagrant-qemu-vm)
            if [ ! -z $2 ]; then
                build_fedora26_vagrant_qemu_vm $2
            else
                echo "ERROR. Vagrant build variables file not specified."
            fi
            ;;
        help|*)
            echo "Usage: $0 {command} option1 option2 ..."
            echo "  Commands                        | Description "
            echo "  -----------------------------     ----------- "
            echo "  new-centos7-vagrant-vmw-vm      : Build CentOS 7 Vagrant box on VMware"
            echo "  new-centos7-vagrant-uefi-vm     : Build CentOS 7 Vagrant UEFI box on KVM-QEMU"
            echo "  new-centos7-vagrant-qemu-vm     : Build CentOS 7 Vagrant box on KVM-QEMU"
            echo "  new-fedora26-vagrant-qemu-vm    : Build Fedora 26 Vagrant box on KVM-QEMU"
            ;;
    esac
}
