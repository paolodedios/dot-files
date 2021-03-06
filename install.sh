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
# Define helper functions
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
    if [ -e ~/.emacs.d/snippets ]; then
        check_list "Private Emacs YASnippet files already installed"
    else
        check_list "Sym-linking shared Emacs YASnippet files to home directory"
        ln -s ~/.bin.shared/etc/snippets ~/.emacs.d/snippets
    fi

    # Delete files that do not exist in the source repo
    check_list "Synchronizing Emacs configuration directory [~/.emacs.d]"
    rsync --exclude ".git/"              \
          --exclude ".hg/"               \
          --exclude ".DS_Store"          \
          --delete-after                 \
          -av .emacs.d/ ~/.emacs.d/config

    if [ "$OS" = "darwin" ]; then
        check_list "Sym-linking init.el to classic .emacs file [~/.emacs.d/config/init.el => ~/.emacs]"
        rm ~/.emacs
        ln -s ~/.emacs.d/config/init.el ~/.emacs
    fi
}

# Update Python configuration files
function update_python_environment()
{
    if [ -e ~/.pip/pip.conf ]; then
        check_list "Private pip configuration already installed"
    else
        check_list "Sym-linking shared pip.conf to home directory"
        mkdir -p ~/.pip
        ln -s ~/.bin.shared/etc/python/pip.conf ~/.pip/pip.conf
    fi

    if [ -e ~/.pydistutils.cfg ]; then
        check_list "Private setup_tools configuration already installed"
    else
        check_list "Sym-linking shared pydistutils.cfg to home directory"
        ln -s ~/.bin.shared/etc/python/pydistutils.conf ~/.pydistutils.cfg
    fi

    if [ -e ~/.buildout/default.cfg ]; then
        check_list "Private zc.buildout configuration already installed"
    else
        check_list "Sym-linking shared buildout.cfg to home directory"
        mkdir -p ~/.buildout
        ln -s ~/.bin.shared/etc/python/buildout.conf ~/.buildout/default.cfg
    fi
}


# Update deployment environment tooling related files
function update_deployment_environment()
{
    # AWS credential files is the standard mechanism for sharing credentials
    # between AWS SDKs, including non-Amazon ones like Boto
    if [ -e ~/.aws/credentials ]; then
        check_list "Private AWS credentials already installed"
    else
        check_list "Sym-linking shared AWS credentials to home directory"
        mkdir -p ~/.aws
        ln -s ~/.bin.shared/etc/aws/credentials.conf ~/.aws/credentials
    fi

    if [ -e ~/.boto ]; then
        check_list "Private Boto configuration already installed"
    else
        check_list "Sym-linking shared Boto configuration to home directory"
        ln -s ~/.bin.shared/etc/aws/boto.conf ~/.boto
    fi

    if [ -e ~/.ansible.cfg ]; then
        check_list "Private ansible configuration already installed"
    else
        check_list "Sym-linking shared ansible configuration to home directory"
        ln -s ~/.bin.shared/etc/ansible/ansible.conf ~/.ansible.cfg
    fi

    if [ -e ~/.vagrant.d/Vagrantfile ]; then
        check_list "Private Vagrantfile configuration already installed"
    else
        check_list "Sym-linking shared Vagrantfile to home directory"
        mkdir -p ~/.vagrant.d
        ln -s ~/.bin.shared/etc/vagrant/Vagrantfile ~/.vagrant.d/Vagrantfile
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
          -av . ~

    echo

    if [ -f ~/.profile ]; then
        check_list "Removing obsolete file [.profile]"
        rm -f ~/.profile
    fi

    if [ -e ~/.bash_extras ]; then
        check_list "Private shell variable file already installed"
    else
        check_list "Sym-linking private shell variable file to home directory"
        ln -s ~/.bin.shared/etc/bash/.bash_extras ~/.bash_extras
    fi

    if [ -e ~/.bash_completion.d ]; then
        check_list "Private bash completion scripts already installed"
    else
        check_list "Sym-linking shared bash completion scripts to home directory"
        ln -s ~/.bin.shared/etc/bash/bash_completion.d ~/.bash_completion.d
       fi

    if [ -e ~/.ssh ]; then
        check_list "Private SSH configuration already installed"
    else
        check_list "Sym-linking shared SSH configuration to home directory"
        ln -s ~/.bin.shared/etc/ssh ~/.ssh
    fi



    update_dev_environment

    update_deployment_environment

    check_list "Synchronize complete"
}

# Update home directory
function backup_home()
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

if [ -d ~/Dropbox ]; then
    check_list  "Dropbox folder found"
else
    error_list  "Drobox folder missing"
fi

if [ -d ~/.bin.shared/bin ]; then
    check_list "Shared bin folder found"
else
    error_list "Shared bin folder missing"
fi

if [ -d ~/.bin.shared/etc ]; then
    check_list "Shared etc folder found"
else
    error_list "Shared etc folder missing"
fi

if [ -d ~/.bin.shared/include ]; then
    check_list "Shared include folder found"
else
    error_list "Shared include folder missing"
fi

if [ -d ~/.bin.shared/lib ]; then
    check_list "Shared lib folder found"
else
    error_list "Shared lib folder missing"
fi

########################################################################################
# Install scripts
########################################################################################

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    update_home
elif [ "$1" == "--dev" -o "$1" == "-d" ]; then
    update_dev_environment
    update_deployment_environment
elif [ "$1" == "--emacs" -o "$1" == "-e" ]; then
    update_emacs_environment
elif [ "$1" == "--backup" -o "$1" == "-b" ]; then
    backup_home
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
        update_home
    else
        error "Aborted"
        exit 1
    fi
fi

unset backup_home
unset update_home
unset update_dev_environment
unset update_deployment_environment
unset update_emacs_environment

cd $current_pwd

notice "Done"
