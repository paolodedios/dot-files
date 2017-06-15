#!/bin/bash
#
# Shell Level Functions
# ---------------------
#
# @author paolodedios
#
########################################################################################


########################################################################################
# Quick system information
########################################################################################

function sysinfo()
{
    echo -e  "Kernel: " $(uname -smr)
    echo -ne "Uptime:  "; uptime
    echo -ne "Time  :  "; date
}

########################################################################################
# Mercurial Functions
########################################################################################

function hg_dirty()
{
    hg status 2> /dev/null | \
      awk '$1 == "?" { unknown = 1 }
           $1 != "?" { changed = 1 }
           END {
             if (changed) printf "*"
             else if (unknown) printf "?"
           }'
}

function hg_in_repo()
{
    [[ $(hg branch 2> /dev/null) ]] && echo "on hg:"
}

function hg_branch()
{
    hg branch 2> /dev/null
}


########################################################################################
# Git Functions
########################################################################################

function git_dirty()
{
    [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}

function git_in_repo()
{
    [[ $(git rev-parse --abbrev-ref HEAD 2> /dev/null) ]] && echo "on git:"
}

function git_branch_name()
{
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(git_dirty)/"
}

# Take repo in $pwd and copy it to the specified location, minus the .git specific files.
function git_export()
{
	mkdir -p "$1"
	git archive master | tar -x -C "$1"
}

# Designate specified git repo URL as the upstream or central repo
function git_add_upstream()
{
    git remote add upstream "$1"
}

# Pull the latest changes from the upstream or central repo
function git_pull_upstream()
{
    git pull upstream master "$1"
}

# Reset last commit. If last commit was pushed upstream then a
# git pull may be necessary before re-committing and pushing
# changes
function git_undo_last_commit()
{
    git reset HEAD~
}

# Discard all local changes in the current branch
function git_reset_branch()
{
    git checkout -- .
}

# Discard all local changes to a file
function git_discard_changes()
{
    git checkout -- "$1"
}

# Use Git's colored diff when available
hash git &>/dev/null
if [ $? -eq 0 ]; then
    function diff() {
		git diff --no-index --color-words "$@"
	}
fi

########################################################################################
# Python Switcher
########################################################################################

function show_python_info()
{
    port select --list python
    port select --list pip
    port select --list virtualenv
}

function select_python26_apple()
{
    sudo port select --set python python26-apple
    sudo port select --set pip none
    sudo port select --set virtualenv none
}

function select_python27_apple()
{
    sudo port select --set python python27-apple
    sudo port select --set pip none
    sudo port select --set virtualenv none
}

function select_python26()
{
    sudo port select --set python python26
    sudo port select --set pip pip26
    sudo port select --set virtualenv virtualenv26
}

function select_python27()
{
    sudo port select --set python python27
    sudo port select --set pip pip27
    sudo port select --set virtualenv virtualenv27
}

function select_python33()
{
    sudo port select --set python python33
    sudo port select --set pip pip33
    sudo port select --set virtualenv virtualenv33
}

function select_python34()
{
    sudo port select --set python python34
    sudo port select --set pip pip34
    sudo port select --set virtualenv virtualenv34
}

function select_python35()
{
    sudo port select --set python python35
    sudo port select --set pip pip35
    sudo port select --set virtualenv virtualenv35
}

function select_python36()
{
    sudo port select --set python python36
    sudo port select --set pip pip36
    sudo port select --set virtualenv virtualenv36
}

########################################################################################
# Python environment utilities
########################################################################################

# Call virtualenvwrapper's "workon" if .venv exists.  This is modified from
# http://justinlilly.com/python/virtualenv_wrapper_helper.html
#
# Also @see
# http://virtualenvwrapper.readthedocs.org/en/latest/tips.html#automatically-run-workon-when-entering-a-directory
py_virtualenv_check()
{
    if [ -e .venv ]; then
        PYTHON_VIRTUALENV_TOPLEVEL=$PWD
        PYTHON_VIRTUALENV_SELECTION=$(cat .venv)
        PYTHON_VIRTUALENV_VERSION=$(python -c 'import sys; print(".".join(map(str, sys.version_info[:3])))')
        if [ "$PYTHON_VIRTUALENV_SELECTION" != "${VIRTUAL_ENV##*/}" ]; then
            echo "Starting virtualenv  : ${PYTHON_VIRTUALENV_SELECTION}"
            echo "Using python version : ${PYTHON_VIRTUALENV_VERSION}"

            if [ ! -d "$WORKON_HOME/$PYTHON_VIRTUALENV_SELECTION" ]; then
                mkvirtualenv $PYTHON_VIRTUALENV_SELECTION
            else
                workon $PYTHON_VIRTUALENV_SELECTION
            fi

            if [ -e .requirements.txt ]; then
                # Warn user of package install/update
                echo "With virtualenv deps :"

                # Print package listing, excluding comment lines, and pipe to
                # sed again to add indenting spaces
                sed -e '/^[[:space:]]*$/d' -e '/^[[:space:]]*#/d' .requirements.txt | sed  's/^/	/'

                # Quitely install/upgrade packages.
                # Redirect warnings and other stdout messages to "1> /dev/null"
                # but don't redirect stderr "2>&1 1> /dev/null"
                pip install -U -r .requirements.txt 1> /dev/null
            fi
        fi
    fi
}

# Override `cd` to use the PYTHON_VIRTUALENV_TOPLEVEL location as the root
# for all "$ cd" commands.  If the toplevel is not defined, the default
# behavior persists.
py_virtualenv_cd()
{
    if [[ $# == 0 ]]; then
        builtin cd $PYTHON_VIRTUALENV_TOPLEVEL
    else
        builtin cd "$@" && py_virtualenv_check
    fi

}

########################################################################################
# Java Switcher
########################################################################################

function select_jdk6()
{
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.6.0)
}

function select_jdk7()
{
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.7.0)
}

function select_jdk8()
{
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.8.0)
}

########################################################################################
# Java Decompiler (JAD) helper command
########################################################################################

# Execute JAD with standard options
function jadexec()
{
    if [ -e $(type -p jad) ]; then
        find . -name \*.class |xargs jad -b -d "$1" -dead -ff -i -o -r -radix10 -s .java -safe -stat
    else
        echo "ERROR: Java Decompiler not found."
    fi
}

# Execute JAD with fully qualified names and with verbose processing output
function jadexecv()
{
    if [ -e $(type -p jad) ]; then
        find . -name \*.class |xargs jad -b -d "$1" -dead -f -ff -i -o -r -radix10 -s .java -safe -stat -v
    else
        echo "ERROR: Java Decompiler not found."
    fi
}


########################################################################################
# Matlab commandline helper
########################################################################################

function matlab_console()
{
    if [ -e $(type -p matlab) ]; then
        matlab -nodisplay -nodesktop -nosplash
    else
        echo "ERROR: Matlab interpreter not found."
    fi
}

function matlab_run_file()
{
    if [ -e $(type -p matlab) ]; then
        matlab -nodisplay -nodesktop -nosplash -r "run('$1'); exit;"
    else
        echo "ERROR: Matlab interpreter not found."
    fi
}


########################################################################################
# File & string-related functions
########################################################################################

# Create a new directory and enter it
function md()
{
	mkdir -p "$@" && cd "$@"
}

# Copy w/ progress
function cp_p()
{
    rsync -WavP --human-readable --progress $1 $2
}

# Find a file with a pattern in name:
function ff()
{
    find . -type f -iname '*'$*'*' -ls ;
}

# Find a directory with a pattern in name:
function fd()
{
   find . -type d -iname '*'$*'*' -ls ;
}

# Case insensitive, excluding svn folders
function fesvn()
{
    find . -path '*/.svn' -prune -o -type f -print0 | xargs -0 grep -I -n -e "$1"
}

# Case insensitive, excluding git folders
function fegit()
{
    find . -path '*/.git*' -prune -o -type f -print0 | xargs -0 grep -I -n -e "$1"
}

# Find a file with pattern $1 in name and Execute $2 on it:
function fe()
{
    find . -type f -iname '*'${1:-}'*' -exec ${2:-file} {} \;  ;
}

# Find a pattern in a set of files and highlight them:
# (needs a recent version of egrep)
function fstr()
{
    OPTIND=1
    local case=""
    local usage="fstr: find string in files. Usage: fstr [-i] \"pattern\" [\"filename pattern\"] "
    while getopts :it opt
    do
        case "$opt" in
        i) case="-i " ;;
        *) echo "$usage"; return;;
        esac
    done
    shift $(( $OPTIND - 1 ))
    if [ "$#" -lt 1 ]; then
        echo "$usage"
        return;
    fi
    find . -type f -name "${2:-*}" -print0 | \
    xargs -0 egrep --color=always -sn ${case} "$1" 2>&- | more

}

# Global search and replace on a directory tree
function gsr
{
    find . -type f -exec sed -i '' "s/$1/$2/g" {} +
}

# Skip the first n lines in a file
function skiphead
{
    n=$(($1 + 1))
    cut -d' ' -f$n-
}

# Cut last n lines in file, 10 by default
function cuttail()
{
    nlines=${2:-10}
    sed -n -e :a -e "1,${nlines}!{P;N;D;};N;ba" $1
}

# Show only the Nth column piped into this function
# e.g. Print the second column of the git status command
#  $ git status -s | col 2
function showcol()
{
    awk -v col=$1 '{print $col}'
}


# Move filenames to lowercase
function lowercase()
{
    for file ; do
        filename=${file##*/}
        case "$filename" in
        */*) dirname==${file%/*} ;;
        *) dirname=.;;
        esac
        nf=$(echo $filename | tr A-Z a-z)
        newname="${dirname}/${nf}"
        if [ "$nf" != "$filename" ]; then
            mv "$file" "$newname"
            echo "lowercase: $file --> $newname"
        else
            echo "lowercase: $file not changed."
        fi
    done
}


# Swap 2 filenames around, if they exist
function swap()
{
    local TMPFILE=tmp.$$

    [ $# -ne 2 ] && echo "swap: 2 arguments needed" && return 1
    [ ! -e $1 ] && echo "swap: $1 does not exist" && return 1
    [ ! -e $2 ] && echo "swap: $2 does not exist" && return 1

    mv "$1" $TMPFILE
    mv "$2" "$1"
    mv $TMPFILE "$2"
}

# Handy Extract command
function extract()
{
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xvjf $1     ;;
             *.tar.gz)    tar xvzf $1     ;;
             *.bz2)       bunzip2 $1      ;;
             *.rar)       unrar x $1      ;;
             *.gz)        gunzip $1       ;;
             *.tar)       tar xvf $1      ;;
             *.tbz2)      tar xvjf $1     ;;
             *.tgz)       tar xvzf $1     ;;
             *.zip)       unzip $1        ;;
             *.Z)         uncompress $1   ;;
             *.7z)        7z x $1         ;;
             *)           echo "'$1' cannot be extracted via func extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# get gzipped size
function gz()
{
	echo "orig size    (bytes): "
	cat "$1" | wc -c
	echo "gzipped size (bytes): "
	gzip -c "$1" | wc -c
}


# find unique occurences in a file
function unique()
{
    sort "$1" | uniq
}


########################################################################################
# Text encoding helpers
########################################################################################

# Escape UTF-8 characters into their 3-byte format
function escape()
{
	printf "\\\x%s" $(printf "$@" | xxd -p -c1 -u)
	echo # newline
}

# Decode \x{ABCD}-style Unicode escape sequences
function unidecode()
{
	perl -e "binmode(STDOUT, ':utf8'); print \"$@\""
	echo # newline
}

# Get a character's Unicode code point
function codepoint()
{
	perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))"
	echo # newline
}


########################################################################################
# Image helpers
########################################################################################

# Image width
function width ()
{
  echo $(sips -g pixelWidth $1 | grep -oE "[[:digit:]]{1,}$")
}

# Image height
function height ()
{
  echo $(sips -g pixelHeight $1 | grep -oE "[[:digit:]]{1,}$")
}

########################################################################################
# Internet/web helpers
########################################################################################

# Syntax-highlight JSON strings or files
# Usage: `json '{"foo":42}'` or `echo '{"foo":42}' | json`
function json()
{
	if [ -p /dev/stdin ]; then
		# piping, e.g. `echo '{"foo":42}' | json`
		python -mjson.tool | pygmentize -l javascript
	else
		# e.g. `json '{"foo":42}'`
		python -mjson.tool <<< "$*" | pygmentize -l javascript
	fi
}

# Create a data URL from a file
function dataurl()
{
	local mimeType=$(file -b --mime-type "$1")
	if [[ $mimeType == text/* ]]; then
		mimeType="${mimeType};charset=utf-8"
	fi
	echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')"
}

# Scrape a URL using wget
function getwebpath()
{
    wget --recursive --level=inf --page-requisites --no-parent --no-clobber --wait=1 $1
}

# Mirror a URL using wget
function mirrorwebsite()
{
    # Equivalent to: --recursive --timestamping --level=inf --no-remove-listing
    wget --mirror --convert-links --html-extension --page-requisites --no-parent --no-clobber --wait=5 $1
}

# List local IP address
function local-ipaddr()
{
    ipconfig getifaddr en0
}

# List all IP address for interfaces
function list-ipaddrs()
{
    ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'
}

# URL-encode strings
function urlencode()
{
    python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"
}

# Download file from URL and save locally using it's remote file name; resume
# broken downloads automatically, retry up to 999 times with exponential backoff,
# follow location redirects; limit download rate to 2MB/sec; alot 8 hours to
# the download before cancelling; do not timeout the retries.
function geturl()
{
    curl --connect-timeout 15   \
         --limit-rate 5M        \
         --max-time 28800       \
         --retry 999            \
         --retry-delay 2        \
         --retry-max-time 0     \
         --location             \
         --remote-name          \
         --continue-at -        \
         $1
}

# All the dig info
function digall()
{
	dig +nocmd "$1" any +multiline +noall +answer
}

########################################################################################
# Process/system related functions
########################################################################################

function pss()
{
    ps $@ -u $USER -o pid,%cpu,%mem,time,command ;
}


function psp()
{
    pss -f | awk '!/awk/ && $0~var' var=${1:-".*"} ;
}

# Kill by process name.
function killps()
{
    local pid pname sig="-TERM"   # Default signal.
    if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
        echo "Usage: killps [-SIGNAL] pattern"
        return;
    fi
    if [ $# = 2 ]; then sig=$1 ; fi
    for pid in $(my_ps| awk '!/awk/ && $0~pat { print $1 }' pat=${!#} ) ; do
        pname=$(my_ps | awk '$1~var { print $5 }' var=$pid )
        if ask "Kill process $pid <$pname> with signal $sig?"
            then kill $sig $pid
        fi
    done
}

# Kill all the tabs in Chrome to free up memory
# Explained:
#   http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
function chromekill()
{
    ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill
}

# Shows most used commands
# http://lifehacker.com/software/how-to/turbocharge-your-terminal-274317.php
function profileme()
{
    history | awk '{print \$2}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr
}

########################################################################################
# Misc utilities
########################################################################################

# Repeat command n times
function repeat()
{
    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do
        eval "$@";
    done
}

# Stopwatch
function timer()
{
    echo "Timer started. Stop with Ctrl-D." && date && time cat && date
}

########################################################################################
# Show definition of function $1
########################################################################################

function showdef()
{
    typeset -f $1
}

function showfuns()
{
    typeset -F | showcol 3 | grep -v _
}

########################################################################################
# Define OS specific functions
########################################################################################

if [ "$OS" = "darwin" ]; then

    # Macports shortcut functions
    function update_macports()
    {
        sudo port selfupdate
        sudo port upgrade outdated
        sudo port clean --all -f installed
        sudo port -f uninstall inactive
    }

    # Reprioritize Time Machine
    function lower-tms-pri()
    {
        echo "Reducing Time Machine priority..."
        sudo renice +5 -p $(ps -axc | grep backupd | awk '{ print \$1 }')
    }

    # Flush Directory Service cache
    function flush-ds()
    {
        dscacheutil -flushcache && killall -HUP mDNSResponder
    }

fi
