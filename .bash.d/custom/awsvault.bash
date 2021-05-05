#!/bin/bash
#
# Helper functions for using aws-vault
#
################################################################################

################################################################################
#
# AWS Vault Helpers
#
# @see https://github.com/99designs/aws-vault
#
################################################################################

#
# Check for 'aws-vault' binary
#
function ensure_aws_vault_tools()
{
    if [ ! $(type -p aws-vault) ]; then
        error "Missing aws-vault executable."
        return 1
    fi
}

#
# List all aws-vault managed profiles and sessions
#
function awsvl()
{
    if ! ensure_aws_vault_tools; then
        return 1
    fi

    aws-vault list --debug
}

#
# Launch aws-vault credentials metadata server
#
# Example Usage:
#
# $ awsvs appliedtheory-service-profile
# Enter token for arn:aws:iam::23456789101:mfa/iam_user: 549236
#
function awsvs()
{
    if ! ensure_aws_vault_tools; then
        return 1
    fi

    #
    # Default to 60 minute assume role TTLs and 12h session TTLs
    # unless specified otherwise by environment variables.
    #
    local aws_assume_role_ttl="${AWS_ASSUME_ROLE_TTL:-60m}"
    local aws_session_ttl="${AWS_SESSION_TTL:-12h}"

    #
    # Use of exec helps reduce the number of processes that are hanging around.
    #
    # @see https://github.com/99designs/aws-vault/blob/master/USAGE.md
    #
    aws-vault exec                                   \
        --server                                     \
        --assume-role-ttl=$aws_assume_role_ttl       \
        --session-ttl=$aws_session_ttl               \
        $1
}


#
# Launch aws-vault execute wrapper.
#
# Example Usage:
#
# $ awsve appliedtheory-admin-profile -- aws s3 ls
# Enter token for arn:aws:iam::23456789101:mfa/iam_user: 549236
#
function awsve()
{
    if ! ensure_aws_vault_tools; then
        return 1
    fi

    #
    # Default to 60 minute assume role TTLs and 12h session TTLs
    # unless specified otherwise by environment variables.
    #
    local aws_assume_role_ttl="${AWS_ASSUME_ROLE_TTL:-60m}"
    local aws_session_ttl="${AWS_SESSION_TTL:-12h}"

    #
    # Use of exec helps reduce the number of processes that are hanging around.
    #
    # @see https://github.com/99designs/aws-vault/blob/master/USAGE.md
    #
    aws-vault exec                                   \
        --assume-role-ttl=$aws_assume_role_ttl       \
        --session-ttl=$aws_session_ttl               \
        $@
}

#
# Set the CHAMBER_KMS_KEY_ALIS to the specified string argument.
#
function chmbre_key()
{
    if ! ensure_aws_vault_tools; then
        return 1
    fi

    if [ -z $1 ]; then
        echo "Removing custom chamber KMS key..."

        unset CHAMBER_KMS_KEY_ALIAS
    else
        echo "Using chamber KMS key : ${1}"

        export CHAMBER_KMS_KEY_ALIAS=$1
    fi
}

#
# Launch chamber vis-a-vis aws-vault
#
# 1. Example Usage (List secrets and expand secret values) :
#
# $ chamber appliedtheory-service-profile list -e <service-name>
# Enter token for arn:aws:iam::23456789101:mfa/iam_user: 549236
# Key         Version                  LastModified      User             Value
# apikey      2                        06-09 17:30:56    admin            apikeyvalue
# other       1                        06-09 17:30:34    admin            othervalue
#
# 2. Example Usage (Execute command with exported aws and secrets in env) :
#
# $ chamber appliedtheory-service-profile exec <service-name> -- <command>
# Enter token for arn:aws:iam::23456789101:mfa/iam_user: 549236
# ...
#
#
function chmbre()
{
    if ! ensure_aws_vault_tools; then
        return 1
    fi

    #
    # First argument must be the aws-vault profile name
    #
    local awsvault_profile=$1
    #
    # Mask profile name from argument list
    #
    shift
    #
    # Execute chamber via 'aws-vault exec'
    #
    awsve $awsvault_profile -- chamber $@
}

#
# Set platform specific environment variables
#
case $OSTYPE in
    darwin*)
        #
        # Under mac OS, aws-vault defaults to using the OS keychain, so default to
        # osascript (native dialog) keychain password prompting instead of terminal
        # based prompting on mac OS.
        #
        export AWS_VAULT_PROMPT=osascript
        #
        # Default to the login keychain for aws-vault.
        #
        export AWS_VAULT_KEYCHAIN_NAME=login
        ;;
    linux*)
        #
        # Under Linux, aws-vault defaults to using encrypted files to allow for
        # terminal and gnome desktop usage, so use terminal based prompting.
        #
        export AWS_VAULT_PROMPT=terminal
        #
        # Force encrypted file as the default
        #
        export AWS_VAULT_BACKEND=file
        ;;
esac
