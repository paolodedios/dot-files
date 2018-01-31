#!/usr/bin/env bash
#
########################################################################################

########################################################################################
# Colored ls
########################################################################################

export LSCOLORS='Gxfxcxdxdxegedabagacad'

########################################################################################
# Set custom theme directory
########################################################################################

if [[ -z "$BASH_IT_CUSTOM_THEME_DIR" ]]; then
    BASH_IT_CUSTOM_THEME_DIR="${BASH_IT_CUSTOM:=${BASH_IT}/custom}/themes"
fi

########################################################################################
# Load the theme
########################################################################################

if [[ $BASH_IT_THEME ]]; then
    if [[ -f $BASH_IT_THEME ]]; then
        source $BASH_IT_THEME
    elif [[ -f "$BASH_IT_CUSTOM_THEME_DIR/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash" ]]; then
        source "$BASH_IT_CUSTOM_THEME_DIR/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash"
    else
        source "$BASH_IT/themes/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash"
    fi
fi
