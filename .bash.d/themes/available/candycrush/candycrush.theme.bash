#!/bin/bash
#
# Command Prompt Customizations
# -----------------------------
#
# @author paolodedios
#
########################################################################################


########################################################################################
# Set terminal variable
########################################################################################

if [[ $COLORTERM = gnome-* && $TERM = xterm ]] && infocmp gnome-256color >/dev/null 2>&1; then
	export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
	export TERM=xterm-256color
fi

########################################################################################
# Display terminal color constants
########################################################################################

function showcolors()
{
    echo
    echo -e "$(tput bold) reg  bld  und   tput-command-colors$(tput sgr0)"

    for i in $(seq 1 256); do
        echo " $(tput setaf $i)Text$(tput sgr0) $(tput bold)"`
            `"$(tput setaf $i)Text$(tput sgr0) $(tput sgr 0 1)"`
            `"$(tput setaf $i)Text$(tput sgr0)  \$(tput setaf $i)"
    done

    echo ' Bold            $(tput bold)'
    echo ' Underline       $(tput sgr 0 1)'
    echo ' Reset           $(tput sgr0)'
    echo
}

########################################################################################
# Set color constants
########################################################################################

if tput setaf 1 &> /dev/null; then

    tput sgr0

    if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
        MAGENTA=$(tput setaf 9)
        ORANGE=$(tput setaf 172)
        YELLOW=$(tput setaf 11)
        GREEN=$(tput setaf 82)
        PURPLE=$(tput setaf 165)
        WHITE=$(tput setaf 256)
    else
        MAGENTA=$(tput setaf 5)
        ORANGE=$(tput setaf 4)
        YELLOW=$(tput setaf 11)
        GREEN=$(tput setaf 2)
        PURPLE=$(tput setaf 1)
        WHITE=$(tput setaf 7)
    fi

    BOLD=$(tput bold)
    RESET=$(tput sgr0)

else
    MAGENTA="\033[1;31m"
    ORANGE="\033[0;33m"
    GREEN="\033[1;32m"
    PURPLE="\033[1;35m"
    WHITE="\033[1;37m"
    BOLD=""
    RESET="\033[m"
fi


function emacs_prompt()
{
    export PS1='\n\u@\h\w $(hg_in_repo)$(hg_branch)$(hg_dirty)$(git_in_repo)$(git_branch_name) \n$ '
}

function nodejs_prompt()
{
    PYTHON_ENV_NAME=$([[ ! -z $VIRTUAL_ENV ]] && echo $(basename $VIRTUAL_ENV) || echo "")

    # Update the prompt with the virtualenv name
    export PS1='\n\[\033[1;32m\](nodemode: ${NAVENAME})\[\033[0m\] '`
              `'$([[ ! -z ${PYTHON_ENV_NAME} ]] && echo "\[\033[1;32m\](pymode: ${PYTHON_ENV_NAME})\[\033[0m\]" || echo"") '`
              `'\[\033[1;31m\]\u\[\033[0m\]@\[\033[1;35m\]\h\[\033[0m\]:\[\033[1;33m\]\w\[\033[0m\] '`
              `'\[\033[1;32m\]$(hg_in_repo)$(hg_branch)$(hg_dirty)$(git_in_repo)$(git_branch_name)\[\033[0m\] \n$ '
}

function def_prompt()
{
    export PS1='\n\[$MAGENTA\]\u\[\033[0m\]@\[$PURPLE\]\h\[\033[0m\]:\[$YELLOW\]\w\[\033[0m\] '`
              `'\[\033[$GREEN\]$(hg_in_repo)$(hg_branch)$(hg_dirty)$(git_in_repo)$(git_branch_name)\[$RESET\] \n$ '
}


if [ ! -z "$INSIDE_EMACS" ]; then
    emacs_prompt
elif [ ! -z "$NAVE" ]; then
    safe_append_prompt_command nodejs_prompt
else
    safe_append_prompt_command def_prompt
fi
