#!/bin/bash
#
# System/shell environment functions
#
########################################################################################


########################################################################################
# File & string-related functions
########################################################################################

# Simulate the 'cd' command using 'pushd'
#
# @see https://mhoffman.github.io/2015/05/21/how-to-navigate-directories-with-the-shell.html#pushd
function pd()
{
    if [ "$#" = "0" ];  then
        pushd ${HOME} > /dev/null 2>&1
    elif [ -f "${1}" ]; then
        ${EDITOR} ${1}
    else
        pushd "$1" > /dev/null 2>&1
    fi
}

# Creates an analogue to the 'cd' command. The 'bd' or back directory
# command allows the caller to navigate up a directory hierarchy using
# the 'popd' command. A numeric argument can be specified which allows
# 'popd' to be called 'n' number times.
#
# @see https://mhoffman.github.io/2015/05/21/how-to-navigate-directories-with-the-shell.html#pushd
function bd()
{
    if [ "$#" = "0" ]; then
        popd > /dev/null 2>&1
    else
        for i in $(seq ${1})
        do
            popd > /dev/null 2>&1
        done
    fi
}

# Create a new directory and enter it
function md()
{
	mkdir -p "$@" && cd "$@"
}

# Copy with progress info
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
# Host file editing aliases
########################################################################################

alias hostedit="sudo vim ${SYS_HOSTFILE}"
alias myhostedit="vim ${MY_HOSTFILE}"

########################################################################################
# Directory navigation; @see .bash_marks
########################################################################################

# Alias the builtin cd command
alias bcd="builtin cd"

########################################################################################
# Networking related aliases
########################################################################################

# Ping and netstat abbreviations
alias ping="ping -c 10"
alias openports="netstat -nap tcp"
alias ns="netstat -alnp tcp | grep -v CLOSE_WAIT | cut -c-6,21-94 | tail +2"

# View HTTP traffic
alias sniff="sudo ngrep -d '$NETIF' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i $NETIF -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

########################################################################################
# Define OS specific functions
########################################################################################

case $OSTYPE in
    darwin*)
        # Create command alias for lsregister
        export CORE_SERVICES_PATH=/System/Library/Frameworks/CoreServices.framework
        export LAUNCH_SERVICES_PATH=Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/
        export LS_REGISTER_PATH=$CORE_SERVICES_PATH/$LAUNCH_SERVICES_PATH

        # Add the VMware ovftool to the PATH
        export VMWARE_PUBLIC_COMMAND_PATH=/Applications/VMware\ Fusion.app/Contents/Public
        export VMWARE_PRIVATE_COMMAND_PATH=/Applications/VMware\ Fusion.app/Contents/Library
        export VMWARE_OVFTOOL_PATH=$VMWARE_PRIVATE_COMMAND_PATH/VMware\ OVF\ Tool
        export PATH=$PATH:$VMWARE_PUBLIC_COMMAND_PATH:$VMWARE_PRIVATE_COMMAND_PATH:$VMWARE_OVFTOOL_PATH

        # VMware docker container engine shortcuts
        function docker_start()
        {
            vctl system start

            echo "Enabling KIND..."
            vctl kind
        }

        function docker_stop()
        {
            vctl system stop
        }

        # Macports shortcut functions
        function update_macports()
        {
            sudo port selfupdate
            sudo port upgrade outdated
            sudo port clean --all -f installed
            sudo port -f uninstall inactive
        }

        # Reprioritize Time Machine
        function lower_tms_pri()
        {
            echo "Reducing Time Machine priority..."
            sudo renice +5 -p $(ps -axc | grep backupd | awk '{ print \$1 }')
        }

        # Flush Directory Service cache
        function flush_ds()
        {
            dscacheutil -flushcache && killall -HUP mDNSResponder
        }

        # Increase the maximum number of open file descriptors to the Mac OS limit
        ulimit -n 2048

        # Add tab completion for `defaults read|write NSGlobalDomain`
        # You could just use `-g` instead, but I like being explicit
        complete -W "NSGlobalDomain" defaults
        ;;

    linux*)

        # Relocate wget-hsts file
        alias wget='wget --hsts-file ~/.config/wget/wget-hsts'

        # Fedora DNF shortcut functions
        function dnf_update()
        {
            sudo dnf update
        }

        function dnf_installed()
        {
            sudo dnf list installed
        }

        function dnf_dbg_install()
        {
            sudo dnf install --enablerepo=fedora-debuginfo --enablerepo= updates-debuginfo
        }

        # Regenerate GRUB
        function rebuild_grub()
        {
            sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg
        }

        # Rebuild initial ramdisk filesystem
        function rebuild_initramfs
        {
            sudo dracut -f --kver `uname -r`
        }
        ;;
esac
