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
# Mercurial aliases
########################################################################################

alias hgstat="hg status"

########################################################################################
# Git/GitHub aliases
########################################################################################

alias gitstat="git status"
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
# Compress/uncompress helpers
########################################################################################

# Gzip-enabled `curl`
alias gurl="curl --compressed"

########################################################################################
# tailoring 'less'
########################################################################################

alias less="less.sh"

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

########################################################################################
# Python virtualenvwrapper aliases
########################################################################################

# Create a new virtual environment
alias pymkenv="mkvirtualenv"

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

# Switch to a specific virtual environment
alias pystartenv="workon"

# Stop using the current virtual environment
alias pystopenv="deactivate && export PYTHON_VIRTUALENV_TOPLEVEL="

# List virtual environments
alias pylsenv="lsvirtualenv -b | sort"

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
alias pyfreezepkgs="pip freeze --local | grep -v '^\-e' > requirements.txt"

# Install python packages from list
alias pyinstallpkgs="pip install -r"

# Upgrade specific package
alias pyupdate='pip install -U'

# Check and activate an environment specified in the current directory
alias pycheckenv="py_virtualenv_check"

########################################################################################
# NodeJS virtualenv aliases
########################################################################################

alias nodestartenv="exec nave use"

alias nodestopenv="exec nave exit"

alias nodecheckenv="nodejs_virtualenv_check"


########################################################################################
# Java related aliases
########################################################################################

alias maven="mvn3"
alias m2="mvn2"
alias m3="mvn3"

########################################################################################
# Grunt related aliases
########################################################################################

# Make Grunt print stack traces by default
command -v grunt > /dev/null && alias grunt="grunt --stack"

########################################################################################
# Declare OS specific aliases
########################################################################################

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

    # Open file in the current Aquamacs window
    alias openwithaquamacs="open -a /Applications/Aquamacs.app $1"

    # Use the emacs binary bundled with Aquamacs for terminal use
    alias emacs="/Applications/Aquamacs.app/Contents/MacOS/bin/emacs"

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
