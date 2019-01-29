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
if [ -d $HOME/.local/etc ]; then
    export PACKER_CONFIG_PATH=$HOME/.local/etc/packer
else
    export PACKER_CONFIG_PATH=/etc/packer
fi

export PACKER_BUILDER_HOME=${PACKER_BUILDER_HOME:-"${PACKER_CONFIG_PATH}"}

# Set PACKER_DATA_PATH variable
if [ -d $HOME/.local/var ]; then
    export PACKER_DATA_PATH=$HOME/.local/var/packer
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
        error "Missing packer builder."
        return 1
    fi
}


function build_centos7_vagrant_vmware_vm()
{
    export VAGRANT_SECURE_KEY="$HOME/.ssh/vagrant_local.pem"
    export PACKER_LOG=0

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    local packer_vagrant_variables="centos-7-x86_64-template-build-config-vagrant.json"
    local packer_vagrant_config="centos-7-x86_64-minimal-docker-vagrant.json"
    local packer_vagrant_vm_build_type="vmware-iso"

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $packer_vagrant_config ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$packer_vagrant_variables                   \
                     -var-file=$1                                                                         \
                     -only=$packer_vagrant_vm_build_type                                                  \
                     $packer_vagrant_config
    else
        echo "ERROR. The command 'atl new-vagrant-vm' must be run from the root of the packer config directory."
    fi
}


function build_centos7_vagrant_uefi_vm()
{
    export VAGRANT_SECURE_KEY="$HOME/.ssh/vagrant_local.pem"
    export PACKER_LOG=0

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
    local kvm_qemu_ovmf_vars_base_file=/usr/share/edk2/ovmf/OVMF_VARS.fd

    cp -f $kvm_qemu_ovmf_vars_base_file $KVM_QEMU_NVRAM_STORAGE_FILE

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    local packer_vagrant_variables="centos-7-x86_64-template-build-config-qemu-vagrant.json"
    local packer_vagrant_vm_build_type="qemu"

    if [ ! -z "$KVM_QEMU_GPU_PCI_ID" -a ! -z "$KVM_QEMU_GPU_AUDIO_PCI_ID" ]; then
        packer_vagrant_config="centos-7-x86_64-minimal-docker-uefi-gpu-vagrant.json"
    else
        packer_vagrant_config="centos-7-x86_64-minimal-docker-uefi-vagrant.json"
    fi

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $packer_vagrant_config ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$packer_vagrant_variables                   \
                     -var-file=$1                                                                         \
                     -only=$packer_vagrant_vm_build_type                                                  \
                     $packer_vagrant_config
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}


function build_centos7_vagrant_qemu_vm()
{
    export VAGRANT_SECURE_KEY="$HOME/.ssh/vagrant_local.pem"
    export PACKER_LOG=0

    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    local packer_vagrant_variables="centos-7-x86_64-template-build-config-qemu-vagrant.json"
    local packer_vagrant_config="centos-7-x86_64-minimal-docker-vagrant.json"
    local packer_vagrant_vm_build_type="qemu"

    echo "using packer var-files located at : [ ${packer_builder_home} ]"

    if [ -f $packer_vagrant_config ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$packer_vagrant_variables                   \
                     -var-file=$1                                                                         \
                     -only=$packer_vagrant_vm_build_type                                                  \
                     $packer_vagrant_config
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}


function build_fedora26_vagrant_qemu_vm()
{
    export VAGRANT_SECURE_KEY="$HOME/.ssh/vagrant_local.pem"
    export PACKER_LOG=0

    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    local packer_vagrant_variables="fedora-26-x86_64-template-build-config-qemu-vagrant.json"
    local packer_vagrant_config="fedora-26-x86_64-minimal-docker-vagrant.json"
    local packer_vagrant_vm_build_type="qemu"

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $packer_vagrant_config ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$packer_vagrant_variables                   \
                     -var-file=$1                                                                         \
                     -only=$packer_vagrant_vm_build_type                                                  \
                     $packer_vagrant_config
    else
        echo "ERROR. This command must be run from the root of the packer config directory."
    fi
}


function build_fedora27_vagrant_qemu_vm()
{
    export VAGRANT_SECURE_KEY="$HOME/.ssh/vagrant_local.pem"
    export PACKER_LOG=0

    echo "Using PACKER_CACHE_DIR            : ${PACKER_CACHE_DIR}"
    echo "Using PACKER_BUILD_DIR            : ${PACKER_BUILD_DIR}"

    # Build a Vagrant VM based on the configuration specified by the
    # PACKER_VAGRANT_CONFIG file. Replace the user defined variables defined in
    # the PACKER_VAGRANT_CONFIG file with those specified in the
    # PACKER_VAGRANT_VARIABLES file. Apply additional variables overrides
    # specified in the command line
    local packer_vagrant_variables="fedora-27-x86_64-template-build-config-qemu-vagrant.json"
    local packer_vagrant_config="fedora-27-x86_64-minimal-docker-vagrant.json"
    local packer_vagrant_vm_build_type="qemu"

    echo "Using packer var-files located at : [ ${PACKER_BUILDER_HOME} ]"

    if [ -f $packer_vagrant_config ]; then
        packer build -var-file=$PACKER_BUILDER_HOME/vars/base/$packer_vagrant_variables                   \
                     -var-file=$1                                                                         \
                     -only=$packer_vagrant_vm_build_type                                                  \
                     $packer_vagrant_config
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
            if [ ! -z "$2" ]; then
                build_centos7_vagrant_vmware_vm "$2"
            else
                echo "ERROR. Vagrant build variables file not specified."
                return 1
            fi
            ;;
        new-centos7-vagrant-uefi-vm)
            if [ ! -z "$2" ]; then
                build_centos7_vagrant_uefi_vm "$2"
            else
                echo "ERROR. Vagrant build variables file not specified."
                return 1
            fi
            ;;
        new-centos7-vagrant-qemu-vm)
            if [ ! -z "$2" ]; then
                build_centos7_vagrant_qemu_vm "$2"
            else
                echo "ERROR. Vagrant build variables file not specified."
                return 1
            fi
            ;;
        new-fedora26-vagrant-qemu-vm)
            if [ ! -z "$2" ]; then
                build_fedora26_vagrant_qemu_vm "$2"
            else
                echo "ERROR. Vagrant build variables file not specified."
                return 1
            fi
            ;;
        new-fedora27-vagrant-qemu-vm)
            if [ ! -z "$2" ]; then
                build_fedora27_vagrant_qemu_vm "$2"
            else
                echo "ERROR. Vagrant build variables file not specified."
                return 1
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
            echo "  new-fedora27-vagrant-qemu-vm    : Build Fedora 27 Vagrant box on KVM-QEMU"
            ;;
    esac

    return $?
}
