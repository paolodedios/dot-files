#!/bin/bash
#
# @author paolodedios
#
########################################################################################

########################################################################################
# Load all shell configuration items in the following order
# 1. bash_extra (private configuration)
# 2. bash_local (local configuration)
########################################################################################

for file in ~/.{bash_extras,bash_local}; do
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

if [ -f /etc/bash_completion.d ]; then
    source /etc/bash_completion.d > /dev/null 2>&1
elif [ -f /opt/local/etc/bash_completion.d ]; then
    source /opt/local/etc/bash_completion.d > /dev/null 2>&1
else
    complete -W "$(echo $(grep '^ssh ' ${HOME}/.bash_history | sort -u | sed 's/^ssh //'))" ssh
fi

########################################################################################
# Check if entering a python virtual environment
########################################################################################

py_virtualenv_check

########################################################################################
# Print system information
########################################################################################

sysinfo
