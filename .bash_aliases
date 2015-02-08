#!/bin/bash
#
# Command Aliases
# ---------------
#
# @author paolodedios
#
########################################################################################

########################################################################################
# Enable aliases to be sudo’ed
########################################################################################

alias sudo="sudo "

########################################################################################
# Always use VIM
########################################################################################

alias vi="vim"

########################################################################################
# Host file editing aliases
########################################################################################

alias hostedit="sudo vim ${SYS_HOSTFILE}"
alias myhostedit="vim ${MY_HOSTFILE}"

########################################################################################
# Mercurial aliases
########################################################################################

alias hgstat="hg status"

########################################################################################
# Git/GitHub aliases
########################################################################################

alias gitstat="git status"
alias git-add-upstream="git remote add upstream"
alias git-pull-upstream="git pull upstream master"

#######################################################################################
# Basic command aliases
#######################################################################################

alias reload-bash="source ~/.bash_profile"

alias jj="jobs -l"

alias hh="history"
alias which="type -a"
alias whichpath="type -p"

alias path="echo -e ${PATH//:/\\n}"
alias libpath="echo -e ${LD_LIBRARY_PATH//:/\\n}"

alias cd="py_virtualenv_cd"
alias cd..="cd .."
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias .="echo $PWD"

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias mkdir="mkdir -p"

alias du="du -kh"
alias df="df -kTh"

alias systail="tail -f /var/log/system.log"

alias untar="tar xvzf"
alias objectdump="od"

# Gzip-enabled `curl`
alias gurl="curl --compressed"

# Make grep more user friendly by highlighting matches and exclude grepping
# through git folders.
alias grep="grep --color=auto"

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
alias map="xargs -n1"

#######################################################################################
# Copies folder and all sub files and folders, preserving security and dates
#######################################################################################

alias cp-folder="cp -Rpv"

#######################################################################################
# Lists folders and files sizes in the current folder
#######################################################################################

alias ducks="du -cksh * | sort -rn | head -11"
alias du1="du -h -d 1"

#######################################################################################
# tailoring 'less'
#######################################################################################

alias less="less.sh"

#######################################################################################
# Prevents accidentally clobbering files.
#######################################################################################

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias mkdir="mkdir -p"

#######################################################################################
# The 'ls' family (this assumes you use a recent GNU ls)
#######################################################################################

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then
	colorflag="--color"                     # GNU `ls`
else
	colorflag="-G"                          # OS X `ls`
fi


alias l="ls -l ${colorflag}"                # List all files colorized in long format
alias ls="command ls ${colorflag}"          # Always use color output for `ls`

alias la="ls -lAxh"                         # show all, including hidden files
alias ll="ls -l"                            # show all, exclude hidden files

alias lx="ls -lxB"                          # sort by extension
alias lk="ls -lSr"                          # sort by size, biggest last
alias lc="ls -ltcr"                         # sort by and show change time, most recent last
alias lu="ls -ltur"                         # sort by and show access time, most recent last
alias lt="ls -ltr"                          # sort by date, most recent last
alias lm="ls -al |more"                     # pipe through "more"
alias lr="ls -lR"                           # recursive ls

alias lsd='ls -l ${colorflag} | grep "^d"'  # List only directories

#######################################################################################
# Networking related aliases
#######################################################################################

alias ping="ping -c 10"
alias openports="netstat -nap tcp"
alias ns="netstat -alnp tcp | grep -v CLOSE_WAIT | cut -c-6,21-94 | tail +2"

alias publicip="curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g'"
alias localip="ipconfig getifaddr $NETIF"

# View HTTP traffic
alias sniff="sudo ngrep -d '$NETIF' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i $NETIF -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

######################################################################################
# Aliases for python and gcc version selection via MacPorts
######################################################################################

alias gcc_list="sudo port select --list gcc"
alias gcc_select="sudo port select --set gcc"

# @see select_python functions in .bash_functions
alias python_list="sudo port select --list python"
alias python_select="sudo port select --set python"

######################################################################################
# Python virtualenvwrapper aliases
######################################################################################

# Create a new virtual environment
alias pymkenv="mkvirtualenv"

# Switch to a specific virtual environment
alias pystartenv="workon"

# Stop using the current virtual environment
alias pystopenv="deactivate"

# List virtual environments
alias pylsenv="lsvirtualenv -b"

# Remove virtual environment
alias pyrmenv="rmvirtualenv"

# Copy virtual environment
alias pycpenv="cpvirtualenv"

# Change dir to top level of current virtual environment
alias pycdenv="cdvirtualenv"

# List site-packages for current virtual environment
alias pylsenvpkgs="lssitepackages"

# Update site-packages via pip
alias pyupdatepkgs='pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U'

# Generate python package list
alias pylspkgs="pip freeze --local > requirements.txt"

# Install python packages from list
alias pyinstallpkgs="pip install -r"


######################################################################################
# Java related aliases
######################################################################################

alias maven="mvn3"
alias m2="mvn2"
alias m3="mvn3"

######################################################################################
# Grunt related aliases
######################################################################################

# Make Grunt print stack traces by default
command -v grunt > /dev/null && alias grunt="grunt --stack"

######################################################################################
# Platform specific aliases
######################################################################################

if [ "$OS" = "darwin" ]; then

    # Clean up LaunchServices to remove duplicates in the “Open With” menu
    alias rebuild-menu="$LS_REGISTER_PATH/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

    # Reboot the Finder
    alias kill-finder="killall Finder && open /System/Library/CoreServices/Finder.app"

    # Reboot the window server/manager and force a logout
    alias kill-windowserver="sudo killall -HUP WindowServer"

    # Cleanup Resource Forks
    alias cleanresforks="find . -name \*._*|xargs \rm"

    # Flush Directory Service cache
    alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

    # Copy emacs configuration to Aquamacs pref directory
    alias refreshaquamacsconfig="cp ~/.emacs ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el"

    # Multiprocess Aquamacs Alias
    alias aquamacs="open -n /Applications/Aquamacs.app"

    # Open file in the current Aquamacs window
    alias openwithaquamacs="open -a /Applications/Aquamacs.app $1"

    # ROT13-encode text. Works for decoding also
    alias rot13="tr a-zA-Z n-za-mN-ZA-M"

    # Hide/show all desktop icons (useful when presenting)
    alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
    alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

    # Disable Spotlight
    alias spotoff="sudo mdutil -a -i off"

    # Enable Spotlight
    alias spoton="sudo mdutil -a -i on"

    # Show program names with lsof
    alias slsof="sudo lsof -i -P"

    # Canonical hex dump; some systems have this symlinked
    command -v hd > /dev/null || alias hd="hexdump -C"

    # OS X has no `md5sum`, so use `md5` as a fallback
    command -v md5sum > /dev/null || alias md5sum="md5"

    # OS X has no `sha1sum`, so use `shasum` as a fallback
    command -v sha1sum > /dev/null || alias sha1sum="shasum"

    # If Matlab is installed, ensure it runs properly on the command line
    command -v matlab > /dev/null && alias matlab="matlab_console"
    command -v matlab > /dev/null && alias matlabex="matlab_run_file"
fi
