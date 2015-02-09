#!/bin/bash
#
# Fork of bashmarks that has mac specific features
# based of https://github.com/huyng/bashmarks
#
########################################################################################

########################################################################################
# USAGE:
# s <bookmark_name>  - Save the current directory as "bookmark_name"
# g <bookmark_name>  - Go (cd) to the directory associated with "bookmark_name"
# d <bookmark_name>  - Delete "bookmark_name"
# l <bookmark_name>  - Lists the specified bookmark associated with "bookmark_name"
# p <bookmark_name>  - Prints the directory associated with "bookmark_name"
# s                  - Saves the default directory
# g                  - Goes to the default directory
# g -                - Goes to the previous directory
# l                  - Lists all available bookmarks
#
# Mac only
# o <bookmark_name>  - Open the directory associated with "bookmark_name" in Finder
# t <bookmark_name>  - Open the directory associated with "bookmark_name" in a new tab
#
########################################################################################


########################################################################################
# Private initializers
########################################################################################

# setup file to store bookmarks
if [ ! -n "$MARKSFILE" ]; then
	MARKSFILE=~/.marks
fi

touch $MARKSFILE

function __unset_dirs()
{
	eval `sed -e 's/export/unset/' -e 's/=.*/;/' ~/.marks | xargs`
}

# safe delete line from sdirs
function __purge_line
{
	if [ -s "$1" ]; then
		# safely create a temp file
		t=$(mktemp -t bashmarks.XXXXXX) || exit 1
		trap "rm -f -- '$t'" EXIT

		# purge line
		sed "/$2/d" "$1" > "$t"
		mv "$t" "$1"

		# cleanup temp file
		rm -f -- "$t"
		trap - EXIT
	fi
}

########################################################################################
# Marking and unmarking functions
########################################################################################

# save current directory to bookmarks
function mark()
{
	check_help $1
	mark_is_valid "$@"

	if [ -z "$exit_message" ]; then
		if [ -z "$@" ]; then
			__purge_line "$MARKSFILE" "export DIR_DEFAULT="
			local CURDIR=$(echo $PWD| sed "s#^$HOME#\$HOME#g")
			echo "export DIR_DEFAULT=\"$CURDIR\"" >> $MARKSFILE
		else
			__purge_line "$MARKSFILE" "export DIR_$1="
			local CURDIR=$(echo $PWD| sed "s#^$HOME#\$HOME#g")
			echo "export DIR_$1=\"$CURDIR\"" >> $MARKSFILE
		fi
	fi
}

# jump to bookmark
function jump()
{
	check_help $1
	source $MARKSFILE
	if [ -z $1 ]; then
		cd "$(eval $(echo echo $(echo \$DIR_DEFAULT)))"
		pwd; $*
	elif [[ "$1" == "-" ]]; then
		cd $1;
		shift; $*
	elif [[ "$1" == ".."  || "$1" == '~' || "$1" == '/' ]]; then
		cd $1;
		pwd; shift; $*
	else
		cd "$(eval $(echo echo $(echo \$DIR_$1)))"
		pwd; shift; $*
	fi
	__unset_dirs
}

# print bookmark
function print_mark()
{
	check_help $1
	source $MARKSFILE
	echo "$(eval $(echo echo $(echo \$DIR_$1)))"
	__unset_dirs
}

# delete bookmark
function unmark()
{
	check_help $1
	mark_is_valid "$@"
	if [ -z "$exit_message" ]; then
		__purge_line "$MARKSFILE" "export DIR_$1="
		unset "DIR_$1"
	fi
	__unset_dirs
}

# print out help for the forgetful
function check_help()
{
	if [ "$1" = "-h" ] || [ "$1" = "-help" ] || [ "$1" = "--help" ] ; then
		echo ''
		echo 's <bookmark_name>  - Save the current directory as "bookmark_name"'
		echo 'g <bookmark_name>  - Go (cd) to the directory associated with "bookmark_name"'
        echo 'd <bookmark_name>  - Deletes"bookmark_name"'
		echo 'l <bookmark_name>  - Lists the bookmark associated with "bookmark_name"'
		echo 'p <bookmark_name>  - Prints the directory associated with "bookmark_name"'

        if [ "`uname`" = "Darwin" ]; then
		echo 'o <bookmark_name>  - Open the directory associated with "name" in Finder'
		echo 't <bookmark_name>  - Open the directory associated with "name" in a new tab'
		fi

		echo 's                  - Saves the default directory'
		echo 'g                  - Goes to the default directory'
		echo 'l                  - Lists all available bookmarks'

		kill -SIGINT $$
	fi
}

# list bookmark and dirname tuples
function marks()
{
	check_help $1
	source $MARKSFILE

	if [  -n "$1" ]; then
		# if color output is not working for you, comment out the line below '\033[1;34m' == "blue"
		env | sort | grep "DIR_$1" |  awk '/DIR_.+/{split(substr($0,5),parts,"="); printf("\033[1;34m%-20s\033[0m %s\n", parts[1], parts[2]);}'
		# uncomment this line if color output is not working with the line above
		# env | grep "^DIR_" | cut -c5-	 | grep "^.*=" | sort
	else
		# if color output is not working for you, comment out the line below '\033[1;34m' == "blue"
		env | sort | awk '/DIR_.+/{split(substr($0,5),parts,"="); printf("\033[1;34m%-20s\033[0m %s\n", parts[1], parts[2]);}'
		# uncomment this line if color output is not working with the line above
		# env | grep "^DIR_" | cut -c5-	 | grep "^.*=" | sort
	fi
	__unset_dirs
}

# list bookmark and dirname tuples without colors
function marks_no_color()
{
	source $MARKSFILE
	env | grep "^DIR_" | cut -c5-	 | grep "^.*=" | sort
	__unset_dirs
}

# list bookmark names only
function list_marks()
{
	source $MARKSFILE
	env | grep "^DIR_" | cut -c5- | sort | grep "^.*=" | cut -f1 -d "="
	__unset_dirs
}

# validate bookmark name
function mark_is_valid()
{
	exit_message=""
	if [ "$1" != "$(echo $1 | sed 's/[^A-Za-z0-9_]//g')" ]; then
		exit_message="Invalid bookmark name: $1"
		echo $exit_message
	fi
}

# bookmark name completion command
function complete_mark
{
	local curw
	COMPREPLY=()
	curw=${COMP_WORDS[COMP_CWORD]}
	COMPREPLY=($(compgen -W '`list_marks`' -- $curw))
	return 0
}


########################################################################################
# Enable Mac OS specific bindings
########################################################################################

if [[ "`uname`" == "Darwin" ]]; then

    # open the specifed bookmark in Finder
    function open_mark_in_finder()
    {
	    if [ -z $1 ]; then
		    open .
		    osascript -e 'tell application "Finder"' -e 'activate' -e 'end tell'
	    else
		    check_help $1
		    source $MARKSFILE
		    open "$(eval $(echo echo $(echo \$DIR_$1)))"
		    cd "$(eval $(echo echo $(echo \$DIR_$1)))"
		    pwd; shift; $*
		    osascript -e 'tell application "Finder"' -e 'activate' -e 'end tell'
	    fi
	    __unset_dirs
    }

    # jump to bookmark in a new tab in the current terminal window
    function open_mark_in_new_tab()
    {
	    check_help $1
	    source $MARKSFILE
	    if [ -z $1 ]; then
		    dst="`pwd`"
	    elif [[ "$1" == "-" || "$1" == ".." || "$1" == '~' ||  "$1" == '/' ]]; then
		    dst="$1";
		    shift
	    else
		    dst="$(eval $(echo echo $(echo \$DIR_$1)))"
		    shift
	    fi

	    if [ $BASHMARK_TERM_APP ]; then
		    current_app="$BASHMARK_TERM_APP"
	    else
		    current_app="$(osascript -e 'tell application "System Events" to get item 1 of (get name of processes whose frontmost is true)')"
	    fi
	    if [ ${current_app:0:5} = "iTerm" ]; then
		    osascript > /dev/null 2>&1 <<APPLESCRIPT
			tell application "${current_app}"
				tell the current terminal
					activate current session
					launch session "${BASHMARKS_ITERM_SESSION:-Default}"
					tell current session
						# does not seem to allow multiple commands
						write text "cd $dst;"
					end tell
				end tell
			end tell
APPLESCRIPT
	    else
	        osascript > /dev/null 2>&1 <<APPLESCRIPT
		tell application "System Events"
				tell process "Terminal" to keystroke "t" using command down
		end tell
		tell application "Terminal"
				activate
				do script with command "cd $dst; $*" in window 1
		end tell
APPLESCRIPT

	    fi
	    __unset_dirs
    }
fi


########################################################################################
# Bind convenience aliases
########################################################################################

alias s="mark"
alias g="jump"
alias d="unmark"
alias l="marks"
alias p="print_mark"

if [[ "`uname`" == "Darwin" ]]; then
    alias o="open_mark_in_finder"
    alias t="open_mark_in_new_tab"
fi


########################################################################################
# Enable tab completion for the mark jump unmark functions
########################################################################################

shopt -s progcomp
complete -F complete_mark g
complete -F complete_mark p
complete -F complete_mark d

if [[ "`uname`" == "Darwin" ]]; then
    complete -F complete_mark o
    complete -F complete_mark t
fi
