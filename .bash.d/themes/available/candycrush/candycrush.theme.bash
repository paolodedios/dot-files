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
    YELLOW="\033[1;33m"
    GREEN="\033[1;32m"
    PURPLE="\033[1;35m"
    WHITE="\033[1;37m"
    ORANGE="\033[1;91m"
    BOLD=""
    RESET="\033[m"
fi


function emacs_prompt()
{
    PS1='\n\u@\h\w $(hg_in_repo)$(hg_branch)$(hg_dirty)$(git_in_repo)$(git_branch_name) \n$ '
}

function prompt_command()
{
    NODEJS_ENV_NAME=$([[ ! -z $NAVE ]] && echo $NAVENAME || echo "")
    PYTHON_ENV_NAME=$([[ ! -z $VIRTUAL_ENV ]] && echo $(basename $VIRTUAL_ENV) || echo "")

    # Update the prompt with the virtualenv name
    PS1="\n"`
       `""$([[ ! -z ${NODEJS_ENV_NAME} ]] && echo "\[$GREEN\](nodemode: ${NAVENAME})\[$RESET\] " || echo "")""`
       `""$([[ ! -z ${PYTHON_ENV_NAME} ]] && echo "\[$GREEN\](pymode: ${PYTHON_ENV_NAME})\[$RESET\] " || echo "")""`
       `"\[${BOLD}${MAGENTA}\]\u\[$WHITE\]@\[$PURPLE\]\h\[$WHITE\]:\[$YELLOW\]\w\[$RESET\] "`
       `"\[$GREEN\]\$(git_in_repo)$(git_branch_name)\[$RESET\] \n\$ "
}


if [ ! -z "$INSIDE_EMACS" ]; then
    safe_append_prompt_command emacs_prompt
else
    safe_append_prompt_command prompt_command
fi
