#
# General Aliases
#
########################################################################################

cite about-alias
about-alias 'general aliases'

########################################################################################
# Sudo alias
########################################################################################

alias _='sudo'

########################################################################################
# Bash-It Aliases
########################################################################################

# Common misspellings of bash-it
alias shit='bash-it'
alias batshit='bash-it'
alias bashit='bash-it'
alias batbsh='bash-it'
alias babsh='bash-it'
alias bash_it='bash-it'
alias bash_ti='bash-it'

# Additional bash-it aliases for help/show
alias bshsa='bash-it show aliases'
alias bshsc='bash-it show completions'
alias bshsp='bash-it show plugins'
alias bshha='bash-it help aliases'
alias bshhc='bash-it help completions'
alias bshhp='bash-it help plugins'
alias bshsch="bash-it search"
alias bshenp="bash-it enable plugin"
alias bshena="bash-it enable alias"
alias bshenc="bash-it enable completion"

########################################################################################
# Directory listing aliases
########################################################################################

if ls --color -d . &> /dev/null; then
    #
    # GNU 'ls'
    #
    # Use --quoting-style=literal or set LC_ALL='C' to prevent
    # multi-word file names from being wrapped in single quotes.
    #
    alias ls="LC_ALL='C' command ls --color=auto -N"
elif ls -G -d . &> /dev/null; then
    #
    # macOS 'ls'; Compact view, show colors
    #
    alias ls='ls -G'
fi

alias sl=ls                    # common mis-spelling
alias l='ls -a'
alias l1='ls -1'

alias la='ls -l -Ah'           # show all, including hidden files
alias ll='ls -l'               # show all, exclude hidden files

alias lx='ls -l -xB'           # sort by extension
alias lk='ls -l -Sr'           # sort by size, biggest last
alias lc='ls -l -tcr'          # sort by and show change time, most recent last
alias lu='ls -l -tur'          # sort by and show access time, most recent last
alias lt='ls -l -tr'           # sort by date, most recent last
alias lm='ls -al | more'       # pipe through 'more'
alias lr='ls -lR'              # recursive ls

alias lsd='ll | grep "^d"'     # List only directories

########################################################################################
# Lists folders and files sizes in the current folder
########################################################################################

alias du='du -kh'
alias df='df -h'
alias ducks='du -cksh * | sort -rn | head -11'
alias du1='du -h -d 1'

########################################################################################
# Filesystem checks
########################################################################################

case $OSTYPE in
    linux*)
        # Linux only, filter out tmpfs mounts using GNU 'df'
        alias dfs='df -h -x tmpfs | grep -v ^none | (read header; echo "$header" ; sort -r -k 7 -i)'
        ;;
esac

########################################################################################
# Colorized Grep
########################################################################################

# Need to check an existing file for a pattern that will be found to ensure
# that the check works when on an OS that supports the color option
if grep --color=auto "a" "${BASH_IT}/"*.md &> /dev/null; then
  alias grep='grep --color=auto'
  export GREP_COLOR='1;33'
fi

########################################################################################
# Miscellaneous
########################################################################################

if which gshuf &> /dev/null; then
    alias shuf=gshuf
fi

alias edit="$EDITOR"
alias pager="$PAGER"

# Shorten clear
alias c='clear'
alias k='clear'
alias cls='clear'

# Shorten exit
alias q='exit'

# Shorten extract
alias xt='extract'

# Uncompress a .tar.gz file
alias untar='tar xvzf'

# Shorten vim
alias vi='vim'

# sudo vim
alias svim='sudo vim'

# Dump binary to console
alias objectdump='od'

# Display whatever file is regular file or folder
function catt()
{
    for i in "$@"; do
        if [ -d "$i" ]; then
            ls "$i"
        else
            cat "$i"
        fi
    done
}

########################################################################################
# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
########################################################################################

alias map='xargs -n1'

########################################################################################
# Shortcuts to edit startup files
########################################################################################

alias vbrc="vim ~/.bashrc"
alias vbpf="vim ~/.bash_profile"

########################################################################################
# Console IRC client
########################################################################################

alias irc="${IRC_CLIENT:=irssi}"

########################################################################################
# Language Aliases
########################################################################################

alias rb='ruby'
alias py='python'
alias ipy='ipython'

########################################################################################
# Pianobar
########################################################################################

# Pianobar can be found here: http://github.com/PromyLOPh/pianobar/
alias piano='pianobar'

########################################################################################
# Directory navigation
########################################################################################

alias cd..='cd ..'             # Common misspelling for going up one directory
alias ..='cd ..'               # Go up one directory
alias ...='cd ../..'           # Go up two directories
alias ....='cd ../../..'       # Go up three directories
alias .....='cd ../../../..'   # Go up four directories
alias ---='cd -'               # Go back

########################################################################################
# Shell History
########################################################################################

# Shell History
alias h='history'

# Job listing
alias j='jobs -l'

########################################################################################
# Tree
########################################################################################

if [ ! -x "$(which tree 2>/dev/null)" ]; then
  alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
fi

########################################################################################
# Directory managment
########################################################################################

alias md='mkdir -p'
alias rd='rmdir'

########################################################################################
# Prevents accidentally clobbering files.
########################################################################################

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias mkdir="mkdir -p"

########################################################################################
# Copies folder and all sub files and folders, preserving security and dates
########################################################################################

alias cp-folder='cp -Rpv'

########################################################################################
# Location shortcuts
########################################################################################

alias which='type -a'
alias whichpath='type -p'
