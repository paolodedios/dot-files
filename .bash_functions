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
    echo -e  "Kernel: " `uname -smr`
    echo -ne "Uptime:  "; uptime
    echo -ne "Time  :  "; date
}

########################################################################################
# Declare helper functions for shell prompt notifications
########################################################################################

# Notice title
function notice()
{
    echo  "\033[1;32m=> $1\033[0m";
}

# Error title
function error()
{
    echo "\033[1;31m=> Error: $1\033[0m";
}

# List item
function check_list()
{
    echo  "  \033[1;32m✔\033[0m $1";
}

# Error list item
function error_list()
{
    echo  "  \033[1;31m✖\033[0m $1";
}

########################################################################################
# Display terminal color constants
########################################################################################

function showcolors()
{
    echo
    echo -e "$(tput bold) reg  bld  und   tput-command-colors$(tput sgr0)"

    for i in $(seq 1 256); do
        echo " $(tput setaf $i)Text$(tput sgr0) $(tput bold)$(tput setaf $i)Text$(tput sgr0) $(tput sgr 0 1)$(tput setaf $i)Text$(tput sgr0)  \$(tput setaf $i)"
    done

    echo ' Bold            $(tput bold)'
    echo ' Underline       $(tput sgr 0 1)'
    echo ' Reset           $(tput sgr0)'
    echo
}

########################################################################################
# Mercurial Functions
########################################################################################

function hg_dirty()
{
    hg status 2> /dev/null \
    | awk '$1 == "?" { unknown = 1 }
           $1 != "?" { changed = 1 }
           END {
             if (changed) printf "*"
             else if (unknown) printf "?"
           }'
}

function hg_in_repo()
{
    [[ `hg branch 2> /dev/null` ]] && echo 'on '
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
    [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working directory clean" ]] && echo "*"
}

function git_branch()
{
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/on \1$(git_dirty)/"
}

# take repo in $pwd and copy it to the specified location, minus the .git specific files.
function gitexport(){
	mkdir -p "$1"
	git archive master | tar -x -C "$1"
}


# Use Git's colored diff when available
hash git &>/dev/null
if [ $? -eq 0 ]; then
    function diff() {
		git diff --no-index --color-words "$@"
	}
fi

######################################################################################
# Java Switcher
######################################################################################

function select_jdk6()
{
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.6.0)
}

function select_jdk7()
{
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.7.0)
}

######################################################################################
# Java Decompiler(JAD) standard command
######################################################################################

# Execute JAD with standard options

function jadexec()
{
    find . -name \*.class |xargs jad -b -d "$1" -dead -ff -i -o -r -radix10 -s .java -safe -stat
}

# Execute JAD with fully qualified names and with verbose processing output

function jadexecv()
{
    find . -name \*.class |xargs jad -b -d "$1" -dead -f -ff -i -o -r -radix10 -s .java -safe -stat -v
}

######################################################################################
# File & string-related functions
######################################################################################

# Create a new directory and enter it
function md()
{
	mkdir -p "$@" && cd "$@"
}

# Copy w/ progress
function cp_p() {
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

# cut last n lines in file, 10 by default
function cuttail()
{
    nlines=${2:-10}
    sed -n -e :a -e "1,${nlines}!{P;N;D;};N;ba" $1
}

# move filenames to lowercase
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

#######################################################################################
# Internet/web helpers
#######################################################################################

# All the dig info
function digall()
{
	dig +nocmd "$1" any +multiline +noall +answer
}

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

# Get a characters Unicode code point
function codepoint()
{
	perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))"
	echo # newline
}

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
function getwebsite()
{
    wget --recursive --level=inf --page-requisites --no-parent --wait=1 $1
}

# Mirror a URL using wget
function mirrorwebsite()
{
    # Equivalent to: --recursive --timestamping --level=inf --no-remove-listing
    wget --mirror --convert-links --html-extension --page-requisites --no-parent --wait=5 $1
}

#######################################################################################
# Process/system related functions
#######################################################################################

function my_ps()
{
    ps $@ -u $USER -o pid,%cpu,%mem,time,command ;
}


function pp()
{
    my_ps -f | awk '!/awk/ && $0~var' var=${1:-".*"} ;
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


#######################################################################################
# Misc utilities
#######################################################################################

function repeat()
{
    # Repeat n times command.
    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do
        eval "$@";
    done
}

function tmslow()
{
    echo "Reducing Time Machine priority..."
    sudo renice +5 -p `ps -axc | grep backupd | awk '{ print \$1 }'`
}

#######################################################################################
# AWS Helpers
#######################################################################################

function aws-usekey()
{
    echo "Using AWS EC2 X.509 private key : [$1]"
    export EC2_PRIVATE_KEY=$1
}

function aws-usecert()
{
    echo "Using AWS EC2 X.509 certificate : [$1]"
    export EC2_CERT=$1
}

function aws-use-endpoint()
{
    echo "Using AWS region endpoint : [$1]"
    export EC2_URL="http://$1"
}
