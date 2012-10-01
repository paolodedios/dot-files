#!/bin/bash
#
# Shell configuration initialization script
#
# @author paolodedios
#
######################################################################################


OS=`uname -s | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`
OSVERSION=`uname -r`; OSVERSION=`expr "$OSVERSION" : '[^0-9]*\([0-9]*\.[0-9]*\)'`
MACHINE=`uname -m | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'`

######################################################################################
# Define helper functions
######################################################################################

# Notice title
function notice()
{
    echo  "\033[1;32m=> $1\033[0m";
}

# Error title
function error()
{
    echo "\033[1;31m=> Error: $1\033[0m";
}

# List item
function check_list()
{
    echo  "  \033[1;32m✔\033[0m $1";
}

# Error list item
function error_list()
{
    echo  "  \033[1;31m✖\033[0m $1";
}

# Check for dependency
function dep()
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

# Update home directory
function updateHome()
{
	rsync --exclude ".git/" --exclude ".hg/" --exclude ".DS_Store" --exclude "install.sh" --exclude "README.md" -av . ~
    echo
    check_list "Synchronize complete"

    if [ -f ~/.profile ]; then
        check_list "Removing obsolete file [.profile]"
        rm -f ~/.profile
    fi

    if [ "$OS" = "darwin" ]; then
        check_list "Copying Aquamacs configuration file [~/.emacs]"
        cp ~/.emacs ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
    fi
}

# Update home directory
function backupHome()
{
    check_list "Backup complete"
}

######################################################################################
# Initialize bootstrap
######################################################################################

current_pwd=$(pwd)
missing=()

######################################################################################
# Validate dependencies
######################################################################################

# Change to the executable's directory
cd "$(dirname "$0")"

notice "Checking dependencies"

dep "git"   "1.7"
dep "hg"    "2.3"
dep "rsync" "3.0"

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


######################################################################################
# Install scripts
######################################################################################

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    notice "Synchronizing configuration files"
    updateHome
elif [ "$1" == "--backup" -o "$1" == "-b" ]; then
    notice "Backing up configuration files"
    backupHome
else
    notice "Synchronizing configuration files"
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
        updateHome
    else
        error "Aborted"
        exit 1
    fi
fi

unset updateHome

cd $current_pwd

notice "Done"