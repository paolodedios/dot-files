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

# Update emacs configuration files
function updateEmacs()
{
    # Copy top-level .emacs config file to $HOME
    check_list "Synchronizing Emacs configuration file [~/.emacs]"
    rsync --exclude ".git/"        \
          --exclude ".hg/"         \
          --exclude ".DS_Store"    \
          -av .emacs ~

    # Delete files that do not exist in the source repo
    check_list "Synchronizing Emacs configuration directory [~/.emacs.d]"
    rsync --exclude ".git/"        \
          --exclude ".hg/"         \
          --exclude ".DS_Store"    \
          --delete-after           \
          -av .emacs.d ~
    
    if [ "$OS" = "darwin" ]; then
        check_list "Copying Emacs configuration to Aquamacs preferences [~/Library/Preferences/Aquamacs Emacs/Preferences.el]"
        cp ~/.emacs ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
    fi
}

# Update home directory
function updateHome()
{
    notice "Synchronizing configuration files"
    
	rsync --exclude ".git/"        \
          --exclude ".hg/"         \
          --exclude ".DS_Store"    \
          --exclude "install.sh"   \
          --exclude "README.md"    \
          --exclude ".emacs*"      \
          -av . ~

    echo

    if [ -f ~/.profile ]; then
        check_list "Removing obsolete file [.profile]"
        rm -f ~/.profile
    fi

    updateEmacs

    check_list "Synchronize complete"
}

# Update home directory
function backupHome()
{
    notice "Backing up configuration files"
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
    updateHome
elif [ "$1" == "--emacs" -o "$1" == "-e" ]; then
    updateEmacs
elif [ "$1" == "--backup" -o "$1" == "-b" ]; then
    backupHome
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
        updateHome
    else
        error "Aborted"
        exit 1
    fi
fi

unset backupHome
unset updateHome
unset updateEmacs

cd $current_pwd

notice "Done"