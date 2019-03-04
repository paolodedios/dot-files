#!/bin/bash
#
# @author paolodedios
#
########################################################################################

########################################################################################
#
# Identify OS and Machine
#
########################################################################################

export OS=$(uname -s | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/')
export OSVERSION=$(uname -r); OSVERSION=$(expr "$OSVERSION" : '[^0-9]*\([0-9]*\.[0-9]*\)')
export MACHINE=$(uname -m | sed -e 's/  */-/g;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/')
export PLATFORM="$MACHINE-$OS-$OSVERSION"

########################################################################################
#
# Run OS indepenedent shell initializations
#
########################################################################################

unset file

#
# Check the window size after each command and update the values of LINES and COLUMNS.
#
shopt -s checkwinsize

#
# Case-insensitive globbing (used in pathname expansion)
#
shopt -s nocaseglob

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

#
# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
#
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null
done


########################################################################################
#
# Configure language variables
#
########################################################################################

#
# Prefer US English and use UTF-8
#
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

########################################################################################
#
# Configure default editor
#
########################################################################################

#
# Make vim the default editor
#
export EDITOR="vim"

########################################################################################
#
# Host file env variables
#
########################################################################################

#
# Put list of remote hosts in $HOME/.hosts
#
export SYS_HOSTFILE=/etc/hosts
export MY_HOSTFILE=$HOME/.hosts

########################################################################################
#
# Command history customization
#
########################################################################################

#
# Append commands to the bash command history file (~/.bash_history) instead of overwriting
#
shopt -s histappend

export HISTSIZE=1048576
export HISTFILESIZE=$HISTSIZE

#
# Avoid succesive duplicates in the bash command history
#
export HISTCONTROL=ignoredups

#
# Make some commands not show up in history
#
export HISTIGNORE="ls:ls *:cd:cd -:pwd;exit:date:* --help"

#
# Force bash to re-read the history file and then append commands to the history
# every time a prompt is shown, instead of after closing the session.
#
export PROMPT_COMMAND='history -a'

#
# Don’t clear the screen after quitting a manual page
#
export MANPAGER="less -X"

#
# Highlight section titles in manual pages
#
export LESS_TERMCAP_md="$ORANGE"

#
# Disable less history file $HOME/.lesshst
#
export LESSHISTFILE=/dev/null

########################################################################################
#
# Run OS specific shell initializations
#
########################################################################################

case $OSTYPE in
    darwin*)
        #
        # Macports installs bash_completion scripts in /opt/local instead of
        # using /etc/profile auto sourcing.
        #
        if [ -f /opt/local/etc/bash_completion ]; then
            source /opt/local/etc/bash_completion > /dev/null 2>&1
        fi

        #
        # tmux sets the TMUX environment variable in tmux sessions, and sets TERM
        # to screen. This isn't a 100% reliable indicator (for example, you can't
        # easily tell if you're running screen inside tmux or tmux inside screen),
        # but it should be good enough in practice.
        #
        if ! { [ -n "$TMUX" ]; } then
            #
            # TMUX session NOT active, set paths
            #
            export SYSTEM_PATH=$PATH
        else
            #
            # TMUX session active, preserve paths
            export SYSTEM_PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/usr/local/MacGPG2/bin
        fi

        #
        # MacPorts versions take precedence
        #
        export PATH=/opt/local/bin:/opt/local/sbin:$SYSTEM_PATH

        #
        # User local binaries, libraries, and configuration files
        #
        export LOCAL_APP_HOME=$HOME/.local

        #
        # Add local libraries to LIBRARY PATH
        #
        if [ -d $LOCAL_APP_HOME/lib ]; then
            #
            # Create local LIB_PATH variable
            #
            export LOCAL_LIB_PATH=$LOCAL_APP_HOME/lib
            #
            # Prepend local libraries to LIBRARY_PATH
            #
            export DYLD_FALLBACK_LIBRARY_PATH=$LOCAL_LIB_PATH:$DYLD_FALLBACK_LIBRARY_PATH
        fi

        #
        # User shared (syncd) binaries, libraries, and configuration files
        #
        export SHARED_APP_HOME=$HOME/.shared

        #
        # Add shared libraries to LIBRARY PATH
        #
        if [ -d $SHARED_APP_HOME/lib ]; then
            #
            # Create shared LIB_PATH variable
            #
            export SHARED_LIB_PATH=$SHARED_APP_HOME/lib
            #
            # Prepend shared libraries to LIBRARY_PATH
            #
            export DYLD_FALLBACK_LIBRARY_PATH=$SHARED_LIB_PATH:$DYLD_FALLBACK_LIBRARY_PATH
        fi

        #
        # Mac OS X search path for shared libraries (from 'man dyld')
        #
        # DYLD_LIBRARY_PATH
        #   This is a colon separated list of directories that contain
        #   libraries. The dynamic linker searches these directories before
        #   it searches the default locations for libraries. It allows you
        #   to test new versions of existing libraries.
        #   For each library that a program uses, the dynamic linker looks
        #   for it in each directory in DYLD_LIBRARY_PATH in turn. If it
        #   still can't find the library, it then searches
        #   DYLD_FALLBACK_FRAMEWORK_PATH and DYLD_FALLBACK_LIBRARY_PATH in
        #   turn.
        #
        # DYLD_FALLBACK_LIBRARY_PATH
        #   This is a colon  separated  list  of  directories  that  contain
        #   libraries.  It is used as the default location for libraries not
        #   found  in  their  install  path.   By  default,  it  is  set  to
        #   $(HOME)/lib:/usr/local/lib:/lib:/usr/lib.
        #
        # The default library search path is only used if DYLD_FALLBACK_LIBRARY_PATH
        # is not set. Setting DYLD_FALLBACK_LIBRARY_PATH will cause the default
        # paths to be ignored, causing lots of problems. If the
        # DYLD_FALLBACK_LIBRARY_PATH is modified, then care must be taken to add the
        # defaults manually. (See man dlopen)
        #
        # Since the shared and local dev environment variables modify the
        # library search path via the DYLD_FALLBACK_LIBRARY_PATH, ensure that
        # it is always first set to the default paths.
        #
        export DYLD_FALLBACK_LIBRARY_PATH=$HOME/lib:/usr/local/lib:/lib:/usr/lib
        ;;

    linux*)
        #
        # tmux sets the TMUX environment variable in tmux sessions, and sets TERM
        # to screen. This isn't a 100% reliable indicator (for example, you can't
        # easily tell if you're running screen inside tmux or tmux inside screen),
        # but it should be good enough in practice.
        #
        if ! { [ -n "$TMUX" ]; } then
           export SYSTEM_PATH=$PATH
        else
           export SYSTEM_PATH=/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin
        fi

        export PATH=$SYSTEM_PATH:/shared/bin

        #
        # User local binaries
        #
        export LOCAL_APP_HOME=$HOME/.local

        #
        # Add shared libraries to LIBRARY PATH
        #
        if [ -d $LOCAL_APP_HOME/lib ]; then
            #
            # Create shared LIB_PATH variable
            #
            export LOCAL_LIB_PATH=$LOCAL_APP_HOME/lib
            #
            # Prepend shared libraries to LIBRARY_PATH
            #
            export LD_LIBRARY_PATH=$LOCAL_LIB_PATH:$LD_LIBRARY_PATH
        fi

        #
        # User shared (syncd) binaries
        #
        export SHARED_APP_HOME=$HOME/.shared
        #
        # Add shared libraries to LIBRARY PATH
        #
        if [ -d $SHARED_APP_HOME/lib ]; then
            #
            # Create shared LIB_PATH variable
            #
            export SHARED_LIB_PATH=$SHARED_APP_HOME/lib
            #
            # Standard Unix search path for shared libraries
            #
            export LD_LIBRARY_PATH=$SHARED_LIB_PATH:$LD_LIBRARY_PATH
        fi
        ;;
esac

########################################################################################
#
# Set bash completion for SSH
#
########################################################################################

#
# Add tab completion for SSH hostnames in .bash_history
#
if [ -f $HOME/.bash_history ]; then
    complete -W "$(echo $(grep '^ssh  ' ${HOME}/.bash_history | sort -u | sed 's/^ssh //'))"  ssh
    complete -W "$(echo $(grep '^scp  ' ${HOME}/.bash_history | sort -u | sed 's/^scp //'))"  scp
    complete -W "$(echo $(grep '^sftp ' ${HOME}/.bash_history | sort -u | sed 's/^sftp //'))" sftp
fi

#
# Add tab completion for SSH hostnames in ~/.ssh/config, ignoring wildcards
#
if [ -e $HOME/.ssh/config ]; then
    complete -o "default"      \
	         -o "nospace"      \
	         -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh
fi

########################################################################################
#
# Set shared library and binary PATH information
#
########################################################################################

#
# $SHARED_APP_HOME binaries should take precedence over system
#
if [ -d $SHARED_APP_HOME/sbin ]; then
    #
    # Create shared bin PATH variable
    #
    export SHARED_SBIN_PATH=$SHARED_APP_HOME/sbin
    #
    # Prepend shared tools to PATH
    #
    export PATH=$SHARED_SBIN_PATH:$PATH
fi

if [ -d $SHARED_APP_HOME/bin ]; then
    #
    # Create shared bin PATH variable
    #
    export SHARED_BIN_PATH=$SHARED_APP_HOME/bin
    #
    # Prepend shared tools to PATH
    #
    export PATH=$SHARED_BIN_PATH:$PATH
fi

#
# Add shared data and log folder to the path, if available
#.
if [ -d $SHARED_APP_HOME/var ]; then
    #
    # Create shared data PATH variable
    #
    export SHARED_LOG_PATH=$SHARED_APP_HOME/var/log
    export SHARED_DATA_PATH=$SHARED_APP_HOME/var/data
fi

########################################################################################
#
# Set local library and binary PATH information
#
########################################################################################

#
# $LOCAL_APP_HOME binaries should take precedence over SHARED_APP_HOME and system
#
if [ -d $LOCAL_APP_HOME/sbin ]; then
    #
    # Create local bin PATH variable
    #
    export LOCAL_SBIN_PATH=$LOCAL_APP_HOME/sbin
    #
    # Prepend local tools to PATH
    #
    export PATH=$LOCAL_SBIN_PATH:$PATH
fi

if [ -d $LOCAL_APP_HOME/bin ]; then
    #
    # Create local bin PATH variable
    #
    export LOCAL_BIN_PATH=$LOCAL_APP_HOME/bin
    #
    # Prepend local tools to PATH
    #
    export PATH=$LOCAL_BIN_PATH:$PATH
fi

#
# Add local data and log folder to the path, if you have it.
#
if [ -d $LOCAL_APP_HOME/var ]; then
    #
    # Create local data PATH variable
    #
    export LOCAL_LOG_PATH=$LOCAL_APP_HOME/var/log
    export LOCAL_DATA_PATH=$LOCAL_APP_HOME/var/data
fi

########################################################################################
#
# User specific environment and program locations
#
########################################################################################

#
# Add project directory
#
export LOCAL_PROJECTS=$HOME/Projects

#
# Declare personal projects directory
#
if [ -d $LOCAL_PROJECTS ]; then
    export PROJECT_HOME=$LOCAL_PROJECTS
fi

########################################################################################
#
# Set TERM variable
#
########################################################################################

if [[ $COLORTERM = gnome-* && $TERM = xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
	export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
	export TERM=xterm-256color
fi

########################################################################################
#
# Print system information
#
########################################################################################

function sysinfo()
{
    echo -e  "Kernel: " $(uname -smr)
    echo -ne "Uptime:  "; uptime
    echo -ne "Time  :  "; date
}

sysinfo
