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
# in OS X, or create a new tab in iTerm) the following files are read
# and run, in this order:
#
# 1. /etc/profile
# 2. /etc/bashrc
# 3. .bash_profile
# 4. .profile
#
# When an interactive, non-login shell is started (when you run "bash" from
# inside a shell, or when you start a shell in XWindows [xterm/gnome-terminal/etc] )
# the following files are read and executed, in this order:
#
# 1. /etc/bashrc
# 2. .bashrc
########################################################################################


######################################################################################
# Load all shell configuration items in the following order
# 1. bash_exports
# 2. bash_functions
# 3. bash_aliases
# 4. bash_prompt
# 5. bash_extra (private configuration)
######################################################################################

for file in ~/.{bash_exports,bash_functions,bash_aliases,bash_prompt,bash_extra}; do
	[ -r "$file" ] && source "$file"
done

########################################################################################
# Run OS specific shell initializations
########################################################################################

if [ "$OS" = "darwin" ]; then

    # Increase the maximum number of open file descriptors to the Mac OS limit
    ulimit -n 2048

    # Add tab completion for `defaults read|write NSGlobalDomain`
    # You could just use `-g` instead, but I like being explicit
    complete -W "NSGlobalDomain" defaults
fi

########################################################################################
# Run OS indepenedent shell initializations
########################################################################################

unset file

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Append commands to the bash command history file (~/.bash_history) instead of overwriting
shopt -s histappend

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null
done

########################################################################################
# Load in bash_completion package
########################################################################################

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion > /dev/null 2>&1
else
    complete -W "$(echo $(grep '^ssh ' .bash_history | sort -u | sed 's/^ssh //'))" ssh
fi

########################################################################################
# Load directory marking and unmarking functions
########################################################################################

source ~/.bash_marks

########################################################################################
# Load git tab completion functions
########################################################################################

source ~/.git_completion

########################################################################################
# Load Python virtualenv wrapper functions
########################################################################################

source virtualenvwrapper.sh > /dev/null 2>&1

########################################################################################
# Check if entering a python virtual environment
########################################################################################

py_virtualenv_check

########################################################################################
# Print system information
########################################################################################

sysinfo
