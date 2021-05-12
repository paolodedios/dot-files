#!/bin/bash
#
# Miscellaneous functions
#
########################################################################################


########################################################################################
#
# Console logging helper functions
#
########################################################################################

function confirm()
{
    # Wait 5 seconds for confirmation from stdin.
    #
    # Defaults to "no" on ENTER key or timeout.
    read -t 10 -r -p "${1:-Are you sure? [y/N]} " response

    case $response in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
}

# Notice title
function notice()
{
    echo -e "\033[1;32m $1\033[0m";
}

# Error title
function error()
{
    echo -e "\033[1;31m Error: $1\033[0m";
}

# Alert title
function alert()
{
    echo -e "\033[1;33m Alert: $1\033[0m";
}

# List item
function check_list()
{
    echo -e "\033[1;32m ✔\033[0m $1";
}

# Error list item
function error_list()
{
    echo -e "\033[1;31m ✖\033[0m $1";
}

########################################################################################
#
# Version check functions
#
# One line version check: bash_require_version_4_0 || return $?
#
########################################################################################

function bash_require_version_4_0()
{
    #
    # Bash 4.0 required for features such as 'readarray'
    #
    if [ $(( ${BASH_VERSINFO[0]:-0} * 10 + ${BASH_VERSINFO[1]:-0})) -lt 40 ] ; then
        error "Bash 4.0+ required shell command. Current shell is Bash '$BASH_VERSION'."
        return 1
    fi
}

function bash_require_version_4_3()
{
    #
    # Bash 4.3 required when declaring local variables as an array or associative array and
    # pass to functions by reference, or when declaring local variables as a named reference
    # of caller arguments for arrays and associative arrays.
    #
    # @see https://mywiki.wooledge.org/BashFAQ/048#The_problem_with_bash.27s_name_references
    #
    if [ $(( ${BASH_VERSINFO[0]:-0} * 10 + ${BASH_VERSINFO[1]:-0})) -lt 43 ] ; then
        error "Bash 4.3+ required shell command. Current shell is Bash '$BASH_VERSION'."
        return 1
    fi
}

function bash_require_version_5_0()
{
    #
    # Bash 5.0 required to for nameref name resolution loop in a function to
    # resolve to a variable by that name in the global scope
    #
    if [ $(( ${BASH_VERSINFO[0]:-0} * 10 + ${BASH_VERSINFO[1]:-0})) -lt 50 ] ; then
        error "Bash 5.0+ required shell command. Current shell is Bash '$BASH_VERSION'."
        return 1
    fi
}
