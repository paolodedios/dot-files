#!/bin/bash
#
# NodeJS/NPM environment variables
#
########################################################################################

########################################################################################
# NodeJS environment variables
########################################################################################

# Set virtualenv working directory, defaults to $HOME/.nave otherwise
export NAVE_DIR=$LOCAL_APP_HOME/bin/nodejs/virtualenvs

########################################################################################
# NodeJS environment utilities
########################################################################################

# Call nave's auto activate function if .naverc exists.
#
# @see https://github.com/isaacs/nave
#
nodejs_virtualenv_check()
{
    if [[ $(type -p nave) && -e .naverc ]]; then
        NODEJS_VIRTUALENV_SELECTION=$(cat .naverc)
        echo "Starting virtualenv  : ${NODEJS_VIRTUALENV_SELECTION}"
        # Use 'exec' for subshell free environments
        # exec nave auto
        nave auto
    fi
}


nodejs_virtualenv_cd()
{
    builtin cd "$@" && nodejs_virtualenv_check
}


########################################################################################
# NodeJS environment aliases
########################################################################################

# Alias the nave environment check command
alias ncd="nodejs_virtualenv_cd"
