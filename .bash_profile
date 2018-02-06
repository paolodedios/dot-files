#!/bin/bash
#
# Login Shell Profile
# -------------------
# Definitions for:
# 1. Login messages
# 2. Shell Prompt
# 3. Shell PATH environment
#
# @author paolodedios
#
########################################################################################

########################################################################################
# When an interactive login shell is started (log in, open terminal or iTerm
# in macOS, or create a new tab in iTerm) the following files are read
# and run, in this order:
#
# 1. /etc/profile
# 2. /etc/bashrc
# 3. $HOME/.bash_profile
# 4. $HOME/.profile
#
# When an interactive, non-login shell is started (when you run "bash" from
# inside a shell, or when you start a shell in XWindows [xterm/gnome-terminal/etc] )
# the following files are read and executed, in this order:
#
# 1. /etc/bashrc
# 2. $HOME/.bashrc
#
# On macOS, this file is configured to be loaded by the bash resource file even for
# non-login shells.
#
########################################################################################

case $OSTYPE in
    linux*)
        # Get the default aliases and functions defined in /etc, as
        # sourced by .bashrc
        source ~/.bashrc
        ;;
esac

########################################################################################
# Set BASH_IT environment variable to the top level path for Bash-It
########################################################################################

export BASH_IT=$HOME/.bash.d

########################################################################################
# Specify a custom theme file located in ${BASH_IT}/themes/
########################################################################################

export BASH_IT_THEME="candycrush"

########################################################################################
# Specify a custom theme location to replace ${BASH_IT}/themes/
########################################################################################

export BASH_IT_CUSTOM_THEME_DIR=$BASH_IT/themes/available

########################################################################################
# Instruct Bash-It to reload itself automatically after enabling or disabling components
#
# Note: Enabling this breaks the github.com/paolodedios/dot-files/install.sh script
#
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1
########################################################################################

########################################################################################
# Plug-in specific configuration environment variables
########################################################################################

# Public/private git repo
export GIT_HOSTING="git@git.domain.com"

# Enable/disable version control status checking within the prompt for all themes
export SCM_CHECK=true

# Set Xterm/screen/Tmux title with only a short hostname, otherwise $HOSTNAME
export SHORT_HOSTNAME=$(hostname -s)

########################################################################################
# Load Bash-It
########################################################################################

source "${BASH_IT}"/main/bash_it.bash

########################################################################################
# Don't check mail when opening terminal.
########################################################################################

unset MAILCHECK
