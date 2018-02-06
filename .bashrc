#!/bin/bash
#
# Login Shell Profile
# -------------------
#
# @author paolodedios
# @see https://github.com/mathiasbynens/dotfiles/commit/3d791926a65118c9119f1f0506e79351dff5dbb7
#
########################################################################################

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

########################################################################################
# On macOS, source .bash_profile even on non-login shells, except when PS1 is
# not set (for non-interactive shells)
#
# This might seem backwards especially given the startup guidelines in the bash manual
# <http://www.gnu.org/software/bash/manual/bashref.html#Bash-Startup-Files> or
# the "INVOCATION" section in the man page. However, a typical developer workflow
# involves some variation of:
#
# * Opening a terminal with shells in tabs
# * Editing code in Vim, and shell out using ":sh"
#
# The initial shells in tabs are login shells, so they source ~/.bash_profile.
# The shells spawned by Vim are not login shells, but they are interactive.
# They look for .bashrc, but not .bash_profile, but because they are interactive,
# PS1 is set, so it is OK to run the shell initialization code.
#
########################################################################################

case $OSTYPE in
    darwin*)
        [ -n "$PS1" ] && source ~/.bash_profile
        ;;
esac
