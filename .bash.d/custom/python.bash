#!/bin/bash
#
# Python shell environment initialization
#
########################################################################################

########################################################################################
# Platform specific environment variables
########################################################################################

# System python installation directory.
export PYTHON_SYSTEM_HOME=/usr/bin

case $OSTYPE in
    darwin*)
        # Non-system python installation directory.
        export PYTHON_ALTINSTALL_HOME=/opt/local

        # Add non-system python to PATH at higher precedence
        export PATH=$PYTHON_ALTINSTALL_HOME/bin:$PATH

        # Set virtualenv working directory to be rooted at $LOCAL_APP_HOME
        export WORKON_HOME=$LOCAL_APP_HOME/bin/python/virtualenvs

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

        alias python_list="sudo port select --list python"

        alias python_select="sudo port select --set python"
        ;;

    linux*)
        # Non-system python installation directory.
        export PYTHON_ALTINSTALL_HOME=$HOME/.local/python

        # Add non-system python to PATH at higher precedence
        export PATH=$PYTHON_ALTINSTALL_HOME/bin:$PATH

        # Set virtualenv working directory to be rooted at $HOME/virtualenvs on
        # multiuser systems.
        export WORKON_HOME=$PYTHON_ALTINSTALL_HOME/virtualenvs
        ;;
esac

########################################################################################
# Python environment variables
########################################################################################

if [ $(type -p virtualenvwrapper.sh) ]; then

    # Make pip use the same directory for virtualenvs as virtualenvwrapper.
    export PIP_VIRTUALENV_BASE=$WORKON_HOME

    # Ensure that pip only runs if there is a virtualenv currently activated.
    export PIP_REQUIRE_VIRTUALENV=true

    # Makes pip detect an active virtualenv and install to it, without
    # having to pass it the -E parameter.
    export PIP_RESPECT_VIRTUALENV=true

    # Declare the two python versions allowed on the system.
    export PYTHON_VIRTUALENV3_VERSION=3.6
    export PYTHON_VIRTUALENV2_VERSION=2.7

    # Ensure that the system version of python is not used to run virtualenv
    # if another one is installed in $PYTHON_ALTINSTALL_HOME.
    #
    # Prefer altinstall Python 3.X over altinstall Python 2.X over system
    # python.
    #
    if [ -f $PYTHON_ALTINSTALL_HOME/bin/python$PYTHON_VIRTUALENV3_VERSION ]; then
        export VIRTUALENVWRAPPER_PYTHON=$PYTHON_ALTINSTALL_HOME/bin/python$PYTHON_VIRTUALENV3_VERSION

        if [ -f $PYTHON_ALTINSTALL_HOME/bin/virtualenv-$PYTHON_VIRTUALENV3_VERSION ]; then
            export VIRTUALENVWRAPPER_VIRTUALENV=$PYTHON_ALTINSTALL_HOME/bin/virtualenv-$PYTHON_VIRTUALENV3_VERSION
        else
            export VIRTUALENVWRAPPER_VIRTUALENV=$PYTHON_ALTINSTALL_HOME/bin/virtualenv
        fi

    elif [ -f $PYTHON_ALTINSTALL_HOME/bin/python$PYTHON_VIRTUALENV2_VERSION ]; then
        export VIRTUALENVWRAPPER_PYTHON=$PYTHON_ALTINSTALL_HOME/bin/python$PYTHON_VIRTUALENV2_VERSION

        if [ -f $PYTHON_ALTINSTALL_HOME/bin/virtualenv-$PYTHON_VIRTUALENV2_VERSION ]; then
            export VIRTUALENVWRAPPER_VIRTUALENV=$PYTHON_ALTINSTALL_HOME/bin/virtualenv-$PYTHON_VIRTUALENV2_VERSION
        else
            export VIRTUALENVWRAPPER_VIRTUALENV=$PYTHON_ALTINSTALL_HOME/bin/virtualenv
        fi
    else
        # Use the system python as a last resort if virtualenv is installed in
        # the system.
        export VIRTUALENVWRAPPER_PYTHON=$PYTHON_SYSTEM_HOME/python
        export VIRTUALENVWRAPPER_VIRTUALENV=$PYTHON_SYSTEM_HOME/virtualenv

    fi

    # Ensure that all new environments are isolated from the system
    # site-packages directory by passing "no-site-packages" as the default
    # argument for virtualenv.
    export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'

    # Load Python virtualenv wrapper functions
    source virtualenvwrapper.sh > /dev/null 2>&1

    # Alias to create a new python 2 virtual environment
    alias py27mkenv="mkvirtualenv --python=${PYTHON_ALTINSTALL_HOME}/bin/python${PYTHON_VIRTUALENV2_VERSION}"

    # Alias to create a new python 3 virtual environment.
    alias py36mkenv="mkvirtualenv --python=${PYTHON_ALTINSTALL_HOME}/bin/python${PYTHON_VIRTUALENV3_VERSION}"
fi


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
    if [[ $(type -p virtualenvwrapper.sh) && -e .venv ]]; then
        PYTHON_VIRTUALENV_TOPLEVEL=$PWD
        PYTHON_VIRTUALENV_SELECTION=$(cat .venv)
        if [ "$PYTHON_VIRTUALENV_SELECTION" != "${VIRTUAL_ENV##*/}" ]; then
            echo "Starting virtualenv  : ${PYTHON_VIRTUALENV_SELECTION}"

            if [ ! -d "$WORKON_HOME/$PYTHON_VIRTUALENV_SELECTION" ]; then
                # Default to Python 3 for new environments
                mkvirtualenv --python=${PYTHON_ALTINSTALL_HOME}/bin/python${PYTHON_VIRTUALENV3_VERSION} $PYTHON_VIRTUALENV_SELECTION
            else
                workon $PYTHON_VIRTUALENV_SELECTION
            fi

            if [ -e .requirements.txt ]; then
                # Warn user of package install/update.
                echo "With virtualenv deps :"

                # Print package listing, excluding comment lines, and pipe to
                # sed again to add indenting spaces.
                sed -e '/^[[:space:]]*$/d' -e '/^[[:space:]]*#/d' .requirements.txt | sed  's/^/	/'

                # Quitely install/upgrade packages.
                #
                # Redirect warnings and other stdout messages to "1> /dev/null"
                # but don't redirect stderr "2>&1 1> /dev/null".
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
# Directory navigation aliases
########################################################################################

# Override the builtin cd with a py virtualenv facade.
alias cd="py_virtualenv_cd"

# Check and activate an environment specified in the current directory.
alias pycheckenv="py_virtualenv_check"

########################################################################################
# Check if entering a python virtual environment
########################################################################################

py_virtualenv_check
