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

for file in ~/.{bash_exports,bash_functions,bash_aliases,bash_prompt,bash_extras}; do
	[ -r "$file" ] && source "$file"
done
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

# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

########################################################################################
# Load in bash-completion package
########################################################################################

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion > /dev/null 2>&1
else
    complete -W "$(echo $(grep '^ssh ' .bash_history | sort -u | sed 's/^ssh //'))" ssh
fi

########################################################################################
# Print system information
########################################################################################

sysinfo

