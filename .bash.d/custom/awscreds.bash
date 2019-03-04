#!/bin/bash
#
# Helper functions for assuming an IAM role via AWS STS (Secure Token Service)
#
################################################################################

################################################################################
#
# AWS Environment Settings
#
################################################################################

#
# AWS_ACCOUNT_NUMBER is an AWS CLI and SDK environment variable and it MUST be
# set before executing 'awscli'
#
export AWS_ACCOUNT_NUMBER=${AWS_ACCOUNT_NUMBER:-"8XXXXXXXXXXX"}
#
# AWS_PROFILE is an AWS CLI and SDK environment variable and it CAN be set as an
# override to profile specification on the command line, otherwise it can be
# provided as a parameter to most 'dockerh' commands.
#
export AWS_PROFILE=${AWS_PROFILE:-""}
#
# AWS_SERVICE_REGION environment variable is a 'dockerh' specific variable that
# CAN be set to use the AWS CLI and SDK environment variable AWS_DEFAULT_REGION
# (AWS_DEFAULT_REGION  superscedes user profile settings). Defaults to 'us-east-1'
#
export AWS_SERVICE_REGION=${AWS_DEFAULT_REGION:-"us-west-2"}
#
# AWS_IAM_ROLE environment variable is a 'dockerh' specific variable that MUST
# be used to specify which role to assume when executing commands given the
# credentials stored in the user specified profile.
#
export AWS_IAM_ROLE=${AWS_IAM_ROLE:-""}

################################################################################
#
# AWS Security Token Service helpers
#
################################################################################

function aws_get_caller_account_number()
{
    local aws_account_number=$(aws sts get-caller-identity --output json | jq -r .Account)

    #
    # Return an empty string if the awscli sts command resulted in a Python
    # exception or credential provider error
    #
    if [[ $aws_account_number == *"Traceback (most recent call last)"* ]]; then
        echo ""
    elif [[ $aws_account_number == *"Unable to locate credentials."* ]]; then
        echo ""
    else
        echo $aws_account_number
    fi
}


function aws_get_temporary_credentials()
{
    local aws_creds_validity_period=3600

    local aws_sts_assumerole_results=$(aws sts assume-role                               \
                                           --duration-seconds $aws_creds_validity_period \
                                           --role-arn $1                                 \
                                           --role-session-name $2                        \
                                           --profile $3  )

    if [ -z "$aws_sts_assumerole_results" ]; then
        error "AWS assume-role command returned no results"
        return 1
    fi

    local aws_sts_role_info=$(echo $aws_sts_assumerole_results   | jq .AssumedRoleUser)
    local aws_sts_credentials=$(echo $aws_sts_assumerole_results | jq .Credentials)

    export AWS_ACCESS_KEY_ID=$(echo $aws_sts_credentials     | jq -r .AccessKeyId)
    export AWS_SECRET_ACCESS_KEY=$(echo $aws_sts_credentials | jq -r .SecretAccessKey)
    export AWS_SESSION_TOKEN=$(echo $aws_sts_credentials     | jq -r .SessionToken)

    echo "AWS Temporary Access Credentials"
    echo "--------------------------------"
    echo "Named Profile         : $3"
    echo "Requested Role        : $1"
    echo "Assumed Role          : $(echo $aws_sts_role_info   | jq -r .AssumedRoleId)"
    echo "Assumed Role Arn      : $(echo $aws_sts_role_info   | jq -r .Arn)"
    echo "Expires on            : $(echo $aws_sts_credentials | jq -r .Expiration)"
    echo "Time now              : $(/bin/date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo

    echo "AWS Credentials"
    echo "---------------"
    echo "AWS_ACCESS_KEY_ID     : $AWS_ACCESS_KEY_ID"
    echo "AWS_SECRET_ACCESS_KEY : $AWS_SECRET_ACCESS_KEY"
    echo "AWS_SESSION_TOKEN     : $AWS_SESSION_TOKEN"
}


function aws_get_access_credentials()
{
    local aws_sts_role_arn="arn:aws:iam::$AWS_ACCOUNT_NUMBER:role/$2"
    local aws_sts_session_name="$2-Session-$(/bin/date +%Y%m%dT%H%M%S)"

    export AWS_USER_PROFILE="$1"

    aws_get_temporary_credentials $aws_sts_role_arn      \
                                  $aws_sts_session_name  \
                                  $AWS_USER_PROFILE

    if [ $? -ne 0 ]; then
        return 1
    fi

    return 0
}


function load_aws_credentials()
{
    # Load user profile from ~/.aws/credentials
    local aws_profile_override=${1:-"$AWS_PROFILE"}

    if [ -z "$aws_profile_override" ]; then
        error "AWS profile not specified."
        return 1
    fi

    # Override the AWS_PROFILE if the caller specified an override
    export AWS_PROFILE=$aws_profile_override

    # Use profile to check for access and secret keys
    local access_key_id=$(aws configure get aws_access_key_id --profile ${aws_profile_override} 2> /dev/null)
    if [ -z $access_key_id ]; then
        error "Invalid AWS profile. Access Key ID not found in profile '${aws_profile_override}'."
        return 1
    fi

    local secret_key=$(aws configure get aws_secret_access_key --profile ${aws_profile_override} 2> /dev/null)
    if [ -z $secret_key ]; then
        error "Invalid AWS profile. Secret Access Key not found in profile '${aws_profile_override}'."
        return 1
    fi

    # Obtain the region information from the profile
    local region_name=$(aws configure get region --profile ${aws_profile_override})

    # If the profile does not define a region use the default AWS_SERVICE_REGION setting
    export AWS_SERVICE_REGION=${region_name:-"$AWS_SERVICE_REGION"}

    # Check if user specified a role that overrides the environment setting
    local aws_iam_role_override=${2:-"$AWS_IAM_ROLE"}

    if [ ! -z "$aws_iam_role_override" ]; then
        # Load user profile from ~/.aws/credentials to obtain temporary
        # credentials for the role specified by the caller or the environment
        # variable AWS_IAM_ROLE.
        echo "AWS IAM Role"
        echo "------------"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_PROFILE           : $aws_profile_override"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_IAM_ROLE          : $aws_iam_role_override"

        aws_get_access_credentials $aws_profile_override $aws_iam_role_override

    else
        # When using AWS_PROFILE, both AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
        # must be unset in the environment in order for AWS_PROFILE to be used.
        #
        # @see https://github.com/aws/aws-cli/issues/3304
        unset AWS_ACCESS_KEY_ID
        unset AWS_SECRET_ACCESS_KEY

        echo "AWS IAM User"
        echo "------------"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_PROFILE           : $aws_profile_override"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_ACCESS_KEY_ID     : $access_key_id"
        echo "AWS_SECRET_ACCESS_KEY : $secret_key"
    fi

    return $?
}


################################################################################
#
# AWS Vault Helpers
#
# @see https://github.com/99designs/aws-vault
#
################################################################################

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
