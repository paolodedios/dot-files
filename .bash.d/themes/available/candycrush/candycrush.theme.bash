#!/bin/bash
#
# Command Prompt Customizations
# -----------------------------
#
# @author paolodedios
#
########################################################################################

########################################################################################
# Set theme variables
########################################################################################

THEME_USER_HOST_SEP="@"
THEME_HOST_PATH_SEP=":"

SCM_CHAR_PREFIX="${bold_white}on "
SCM_CHAR_SUFFIX=" ${green}⎇  ${bold_green}["

SCM_GIT_CHAR="${SCM_CHAR_PREFIX}${bold_green}git${SCM_CHAR_SUFFIX}"
SCM_HG_CHAR="${SCM_CHAR_PREFIX}${bold_green}hg${SCM_CHAR_SUFFIX}"
SCM_SVN_CHAR="${SCM_CHAR_PREFIX}${bold_green}svn${SCM_CHAR_SUFFIX}"
SCM_NONE_CHAR=""

SCM_GIT_AHEAD_CHAR="${bold_green}↑"
SCM_GIT_BEHIND_CHAR="${bold_red}↓"
SCM_GIT_STAGED_CHAR="${bold_yellow}+"
SCM_GIT_UNSTAGED_CHAR="${bold_purple}⟐"
SCM_GIT_UNTRACKED_CHAR="${bold_red}◎"

SCM_THEME_PROMPT_DIRTY=""
SCM_THEME_PROMPT_CLEAN=""

SCM_THEME_PROMPT_PREFIX="${bold_green}"
SCM_THEME_PROMPT_SUFFIX="${bold_green}]${reset_color}"

########################################################################################
# Prompt Generators
########################################################################################

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
       `""$([[ ! -z ${NODEJS_ENV_NAME} ]] && echo "${bold_blue}(jsenv: ${NODEJS_ENV_NAME})${reset_color} " || echo "")""`
       `""$([[ ! -z ${PYTHON_ENV_NAME} ]] && echo "${bold_cyan}(pyenv: ${PYTHON_ENV_NAME})${reset_color} " || echo "")""`
       `"${bold_red}\u${bold_white}${THEME_USER_HOST_SEP}${bold_purple}\h${bold_white}${THEME_HOST_PATH_SEP}${bold_yellow}\w${reset_color} "`
       `"$(scm_prompt_char_info)${reset_color} \n\$ "
}

########################################################################################
# Load Prompt
########################################################################################

if [ ! -z "$INSIDE_EMACS" ]; then
    safe_append_prompt_command emacs_prompt
else
    safe_append_prompt_command prompt_command
fi
