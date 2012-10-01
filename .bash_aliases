#!/bin/bash
#
# Command Aliases
# ---------------
#
# @author paolodedios
#
########################################################################################

########################################################################################
# Always use VIM
########################################################################################

alias vi='vim'

########################################################################################
# Mercurial aliases
########################################################################################

alias hgstat='hg status'

########################################################################################
# Git/GitHub aliases
########################################################################################

alias gitstat='git status'
alias git-add-upstream='git remote add upstream'
alias git-pull-upstream='git pull upstream master'

#######################################################################################
# Basic command aliases
#######################################################################################

alias reload-bash='source ~/.profile'

alias jj='jobs -l'

alias hh='history'
alias which='type -a'
alias whichpath='type -p'

alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'

alias cd..='cd ..'
alias ..='cd ..'
alias .='echo $PWD'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

alias du='du -kh'
alias df='df -kTh'

alias systail='tail -f /var/log/system.log'

alias untar='tar xvzf'
alias objectdump='od'

# Make grep more user friendly by highlighting matches and exclude grepping
# through git folders.
alias grep='grep --color=auto'

#######################################################################################
# Copies folder and all sub files and folders, preserving security and dates
#######################################################################################

alias cp-folder="cp -Rpv" 

#######################################################################################
# Shows most used commands
# http://lifehacker.com/software/how-to/turbocharge-your-terminal-274317.php
#######################################################################################

alias profileme="history | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"
 
#######################################################################################
# Lists folders and files sizes in the current folder
#######################################################################################

alias ducks='du -cksh * | sort -rn | head -11'
alias du1='du -h -d 1'

#######################################################################################
# tailoring 'less'
#######################################################################################

alias more='less'

#######################################################################################
# Prevents accidentally clobbering files.
#######################################################################################

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'

#######################################################################################
# The 'ls' family (this assumes you use a recent GNU ls)
#######################################################################################

alias la='ls -lAxh'                 # show all, including hidden files
alias ll='ls -l'                    # show all, exclude hidden files

alias lx='ls -lxB'                  # sort by extension
alias lk='ls -lSr'                  # sort by size, biggest last
alias lc='ls -ltcr'                 # sort by and show change time, most recent last
alias lu='ls -ltur'                 # sort by and show access time, most recent last
alias lt='ls -ltr'                  # sort by date, most recent last
alias lm='ls -al |more'             # pipe through 'more'
alias lr='ls -lR'                   # recursive ls

#######################################################################################
# Networking related aliases
#######################################################################################

alias ping='ping -c 10'
alias openports='netstat -nap tcp'
alias ns='netstat -alnp tcp | grep -v CLOSE_WAIT | cut -c-6,21-94 | tail +2'

######################################################################################
# Aliases for python and gcc version selection via MacPorts
######################################################################################

alias python_list='sudo port select --list python'
alias python_select='sudo port select --set python'

alias gcc_list='sudo port select --list gcc'
alias gcc_select='sudo port select --set gcc'

######################################################################################
# Java related aliases
######################################################################################

alias maven='mvn3'
alias m2='mvn2'
alias m3='mvn3'

######################################################################################
# Python virtualenvwrapper aliases
######################################################################################

# create a new virtual environment
alias pymkenv='mkvirtualenv'

# switch to a specific virtual environment
alias pystartenv='workon'

# stop using the current virtual environment
alias pystopenv='deactivate'

# list virtual environments
alias pylsenv='lsvirtualenv'

# remove virtual environment
alias pyrmenv='rmvirtualenv'

# copy virtual environment
alias pycpenv='cpvirtualenv'

# change dir to top level of current virtual environment
alias pycdenv='cdvirtualenv'

# list site-packages for current virtual environment
alias pylsenvpkgs='lssitepackages'

######################################################################################
# Platform specific aliases
######################################################################################

if [ "$OS" = "darwin" ]; then
    
    alias rebuild-menu='$LS_REGISTER_PATH/lsregister -kill -r -domain local -domain system -domain user'

    # Cleanup Resource Forks
    alias cleanresforks='find . -name \*._*|xargs \rm'

    # Octave CLI
    alias octave="exec '/Applications/Octave.app/Contents/Resources/bin/octave'"

    # Copy emacs configuration to Aquamacs pref directory
    alias refreshaquamacsconfig='cp ~/.emacs ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el'

    # Multiprocess Aquamacs Alias
    alias aquamacs="exec '/Applications/Aquamacs.app/Contents/MacOS/Aquamacs'"

    # Open file in the current Aquamacs window
    alias openwithaquamacs='open -a /Applications/Aquamacs.app $1'
fi