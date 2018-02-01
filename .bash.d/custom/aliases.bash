#!/bin/bash
#
# Command Aliases
# ---------------
#
# @author paolodedios
#
########################################################################################

########################################################################################
# Host file editing aliases
########################################################################################

alias hostedit="sudo vim ${SYS_HOSTFILE}"
alias myhostedit="vim ${MY_HOSTFILE}"

########################################################################################
# Git/GitHub aliases
########################################################################################

alias gitaddup="git_add_upstream"
alias gitpullup="git_pull_upstream"
alias gitreset="git_reset_branch"
alias gitdiscard="git_discard_changes"

########################################################################################
# Directory navigation; @see .bash_marks
########################################################################################

# Alias the builtin cd command
alias bcd="builtin cd"

# Alias the nave environment check command
alias ncd="nodejs_virtualenv_cd"

# Override the builtin cd with a py virtualenv facade
alias cd="py_virtualenv_cd"

########################################################################################
# Networking related aliases
########################################################################################

alias ping="ping -c 10"
alias openports="netstat -nap tcp"
alias ns="netstat -alnp tcp | grep -v CLOSE_WAIT | cut -c-6,21-94 | tail +2"

alias publicip="curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g'"
alias localip="ipconfig getifaddr $NETIF"

# View HTTP traffic
alias sniff="sudo ngrep -d '$NETIF' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i $NETIF -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

########################################################################################
# Aliases for python and gcc version selection via MacPorts
########################################################################################

alias gcc_list="sudo port select --list gcc"
alias gcc_select="sudo port select --set gcc"

# @see select_python functions in .bash_functions
alias python_list="sudo port select --list python"
alias python_select="sudo port select --set python"

# Check and activate an environment specified in the current directory
alias pycheckenv="py_virtualenv_check"

# Create a new python 2.6 virtual environment
alias py26mkenv="mkvirtualenv --python=/opt/local/bin/python2.6"

# Create a new python 2.7 virtual environment
alias py27mkenv="mkvirtualenv --python=/opt/local/bin/python2.7"

# Create a new python 3.4 virtual environment
alias py34mkenv="mkvirtualenv --python=/opt/local/bin/python3.4"

# Create a new python 3.5 virtual environment
alias py35mkenv="mkvirtualenv --python=/opt/local/bin/python3.5"

# Create a new python 3.6 virtual environment
alias py36mkenv="mkvirtualenv --python=/opt/local/bin/python3.6"
