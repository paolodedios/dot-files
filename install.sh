#!/bin/bash
#
# Shell configuration initialization script
#
# @author paolodedios
#
########################################################################################


OS=`uname -s | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`
OSVERSION=`uname -r`; OSVERSION=`expr "$OSVERSION" : '[^0-9]*\([0-9]*\.[0-9]*\)'`
MACHINE=`uname -m | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`

########################################################################################
# Define UI helper functions
########################################################################################

# Notice title
function notice()
{
    echo -e "\033[1;32m $1\033[0m";
}

# Error title
function error()
{
    echo -e "\033[1;31m ERROR: $1\033[0m";
}

# Alert title
function alert()
{
    echo -e "\033[1;33m ALERT: $1\033[0m";
}

# List item
function check_list()
{
    echo -e "\033[1;32m ✔\033[0m $1";
}

# Error list item
function error_list()
{
    echo -e "\033[1;31m ✖\033[0m $1";
}

########################################################################################
# Define helper functions
########################################################################################

# Check for dependency
function check_deps()
{
    # Check installed
    local i=true
    type -p $1 &> /dev/null || i=false

    # Check version
    if $i ; then
        local version=$($1 --version | grep -oE -m 1 "[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+\.?[[:digit:]]?")
        [[ $version < $2 ]] && local msg="$1 version installed: [ $version ], version needed: [ $2 ]"
    else
        local msg="Missing $1"
    fi

    # Save if dep not met
    if ! $i || [ -n "$msg" ] ; then
        missing+=($msg)
    fi
}

# Update emacs configuration files
function update_emacs_environment()
{
    if [ -e $HOME/.emacs.d/snippets ]; then
        check_list "Private Emacs YASnippet files already installed"
    else
        check_list "Sym-linking shared Emacs YASnippet files to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/snippets $HOME/.emacs.d/snippets
    fi

    # Delete files that do not exist in the source repo
    check_list "Synchronizing Emacs configuration directory [~/.emacs.d]"
    rsync --exclude ".git/"              \
          --exclude ".hg/"               \
          --exclude ".DS_Store"          \
          --delete-after                 \
          -av .emacs.d/ $HOME/.emacs.d/config

    if [ "$OS" = "darwin" ]; then
        check_list "Sym-linking init.el to classic .emacs file [~/.emacs.d/config/init.el => ~/.emacs]"
        rm $HOME/.emacs
        ln -s $HOME/.emacs.d/config/init.el $HOME/.emacs
    fi
}

# Update Python configuration files
function update_python_environment()
{
    if [ -e $HOME/.pip/pip.conf ]; then
        check_list "Private pip configuration already installed"
    else
        check_list "Sym-linking shared pip.conf to home directory"
        mkdir -p $HOME/.pip
        ln -s $HOME/$SHARED_FOLDER/etc/python/pip.conf $HOME/.pip/pip.conf
    fi

    if [ -e $HOME/.pydistutils.cfg ]; then
        check_list "Private setup_tools configuration already installed"
    else
        check_list "Sym-linking shared pydistutils.cfg to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/python/pydistutils.conf $HOME/.pydistutils.cfg
    fi

    if [ -e $HOME/.buildout/default.cfg ]; then
        check_list "Private zc.buildout configuration already installed"
    else
        check_list "Sym-linking shared buildout.cfg to home directory"
        mkdir -p $HOME/.buildout
        ln -s $HOME/$SHARED_FOLDER/etc/python/buildout.conf $HOME/.buildout/default.cfg
    fi
}


# Update deployment environment tooling related files
function update_deployment_environment()
{
    # AWS credential files is the standard mechanism for sharing credentials
    # between AWS SDKs, including non-Amazon ones like Boto
    if [ -e $HOME/.aws/credentials ]; then
        check_list "Private AWS credentials already installed"
    else
        check_list "Sym-linking shared AWS credentials to home directory"
        mkdir -p $HOME/.aws
        ln -s $HOME/$SHARED_FOLDER/etc/aws/credentials.conf $HOME/.aws/credentials
    fi

    if [ -e $HOME/.boto ]; then
        check_list "Private Boto configuration already installed"
    else
        check_list "Sym-linking shared Boto configuration to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/aws/boto.conf $HOME/.boto
    fi

    if [ -e $HOME/.ansible.cfg ]; then
        check_list "Private ansible configuration already installed"
    else
        check_list "Sym-linking shared ansible configuration to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/ansible/ansible.conf $HOME/.ansible.cfg
    fi

    if [ -e $HOME/.vagrant.d/Vagrantfile ]; then
        check_list "Private Vagrantfile configuration already installed"
    else
        check_list "Sym-linking shared Vagrantfile to home directory"
        mkdir -p $HOME/.vagrant.d
        ln -s $HOME/$SHARED_FOLDER/etc/vagrant/Vagrantfile $HOME/.vagrant.d/Vagrantfile
    fi
}


# Update development environment
function update_dev_environment()
{
    update_python_environment

    update_emacs_environment
}


# Update home directory
function update_home()
{
    notice "Synchronizing configuration files"

	rsync --exclude ".git/"        \
          --exclude ".hg/"         \
          --exclude ".DS_Store"    \
          --exclude "install.sh"   \
          --exclude "README.md"    \
          --exclude "LICENSE"      \
          --exclude ".emacs*"      \
          -av . $HOME

    echo

    if [ -f $HOME/.profile ]; then
        check_list "Removing obsolete file [.profile]"
        rm -f $HOME/.profile
    fi

    if [ -e $HOME/.bash_extras ]; then
        check_list "Private shell variable file already installed"
    else
        check_list "Sym-linking private shell variable file to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/bash/.bash_extras $HOME/.bash_extras
    fi

    if [ -e $HOME/.bash_completion.d ]; then
        check_list "Private bash completion scripts already installed"
    else
        check_list "Sym-linking shared bash completion scripts to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/bash/bash_completion.d $HOME/.bash_completion.d
       fi

    if [ -e $HOME/.ssh ]; then
        check_list "Private SSH configuration already installed"
    else
        check_list "Sym-linking shared SSH configuration to home directory"
        ln -s $HOME/$SHARED_FOLDER/etc/ssh $HOME/.ssh
    fi

    update_dev_environment

    update_deployment_environment

    check_list "Synchronize complete"
}

# Update home directory
function update_backups()
{
    notice "Backing up configuration files"
    check_list "Backup complete"
}

########################################################################################
# Initialize bootstrap
########################################################################################

current_pwd=$(pwd)
missing=()

########################################################################################
# Validate dependencies
########################################################################################

# Change to the executable's directory
cd "$(dirname "$0")"

notice "Checking dependencies"

check_deps "git"   "1.7"
check_deps "rsync" "3.0"

if [ "${#missing[*]}" -gt "0" ]; then

  error "Missing dependencies"

  arr=("${missing[*]}")
  for need in ${!arr[*]}
  do
      error_list "${arr[$need]}"
  done

  exit 1
else
    check_list "Dependencies found"
fi

# Check for local binaries and config files
if [ -d $HOME/.bin.local ]; then
    check_list "Local folder found"
    LOCAL_FOLDER=".bin.local"
elif [ -d $HOME/.local ]; then
    check_list "Local folder found"
    LOCAL_FOLDER=".local"
else
    error_list "Local folder missing"
fi

if [ -d $HOME/$LOCAL_FOLDER/bin ]; then
    check_list "Local bin folder found"
else
    error_list "Local bin folder missing"
fi

if [ -d $HOME/$LOCAL_FOLDER/etc ]; then
    check_list "Local etc folder found"
else
    error_list "Local etc folder missing"
fi

# Check for shared binaries and config files
if [ -d $HOME/.bin.shared ]; then
    check_list "Shared folder found"
    SHARED_FOLDER=".bin.shared"
elif [ -d $HOME/.shared ]; then
    check_list "Shared folder found"
    SHARED_FOLDER=".shared"
else
    error_list "Shared folder missing"
fi

if [ -d $HOME/$SHARED_FOLDER/bin ]; then
    check_list "Shared bin folder found"
else
    error_list "Shared bin folder missing"
fi

if [ -d $HOME/$SHARED_FOLDER/etc ]; then
    check_list "Shared etc folder found"
else
    error_list "Shared etc folder missing"
fi

########################################################################################
# Install scripts
########################################################################################

function show_usage()
{
    notice "Configuration File Installer Usage"
    notice "$0 [arguments] \n"

    echo "Arguments:"
    echo "--help   (-h): Display this help message"
    echo "--force  (-f): Force install of all settings without confirmation"
    echo "--dev    (-d): Install development environment files only"
    echo "--emacs  (-e): Install emacs config files only"
    echo "--backup (-b): Backup current settings"
    exit 0
}

# Process long form options
for param in "$@"; do
    shift
    case "$param" in
        "--help")
            set -- "$@" "-h"
            ;;
        "--force")
            set -- "$@" "-f"
            ;;
        "--dev")
            set -- "$@" "-d"
            ;;
        "--emacs")
            set -- "$@" "-e"
            ;;
        "--backups")
            set -- "$@" "-b"
            ;;
        *)
            set -- "$@" "$param"
            ;;
    esac
done

OPTIND=1
while getopts "hfdeb?" opt; do
    case "$opt" in
        "h")
            show_usage
            exit 0
            ;;
        "f")
            force_install=true
            ;;
        "d")
            dev_install=true
            ;;
        "e")
            emacs_install=true
            ;;
        "b")
            create_backups=true
            ;;
        "?")
            show_usage >&2
            exit 1
            ;;
    esac
done
shift $(expr $OPTIND - 1)


if [ $force_install ]; then
    #
    # Force update home directory
    #
    update_home

elif [ $dev_install ]; then
    #
    # Update development environment files
    #
    update_dev_environment
    update_deployment_environment

elif [ $emacs_install ]; then
    #
    # Update emacs configuration
    #
    update_emacs_environment

elif [ $create_backups ]; then
    #
    # Create new backup
    #
    update_backups

else
    #
    # Confirm home directory update
    #
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
        update_home
    else
        error "Aborted"
        exit 1
    fi
fi

# Cleanup environment
unset update_backups
unset update_home
unset update_dev_environment
unset update_deployment_environment
unset update_emacs_environment

cd $current_pwd

notice "Done"
