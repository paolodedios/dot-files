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
# set before executing 'dockerh'
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
export AWS_SERVICE_REGION=${AWS_DEFAULT_REGION:-"us-east-1"}
#
# AWS_IAM_ROLE environment variable is a 'dockerh' specific variable that MUST
# be used to specify which role to assume when executing commands given the
# credentials stored in the user specified profile.
#
export AWS_IAM_ROLE=${AWS_IAM_ROLE:-""}

################################################################################
#
# Utility functions
#
################################################################################

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
    echo -e " \033[1;32m $1\033[0m";
}

# Error title
function error()
{
    echo -e " \033[1;31m Error: $1\033[0m";
}

# List item
function check_list()
{
    echo -e "  \033[1;32m✔\033[0m $1";
}

# Error list item
function error_list()
{
    echo -e "  \033[1;31m✖\033[0m $1";
}

################################################################################
#
# AWS Security Token Service helpers
#
################################################################################


function aws_get_temporary_credentials()
{
    local AWS_CREDS_VALIDITY_PERIOD=3600

    local AWS_STS_ASSUMEROLE_RESULTS=$(aws sts assume-role                               \
                                           --duration-seconds $AWS_CREDS_VALIDITY_PERIOD \
                                           --role-arn $1                                 \
                                           --role-session-name $2                        \
                                           --profile $3  )

    if [ -z "$AWS_STS_ASSUMEROLE_RESULTS" ]; then
        error "AWS assume-role command returned no results"
        return 1
    fi

    local AWS_STS_ROLE_INFO=$(echo $AWS_STS_ASSUMEROLE_RESULTS   | jq .AssumedRoleUser)
    local AWS_STS_CREDENTIALS=$(echo $AWS_STS_ASSUMEROLE_RESULTS | jq .Credentials)

    export AWS_ACCESS_KEY_ID=$(echo $AWS_STS_CREDENTIALS     | jq -r .AccessKeyId)
    export AWS_SECRET_ACCESS_KEY=$(echo $AWS_STS_CREDENTIALS | jq -r .SecretAccessKey)
    export AWS_SESSION_TOKEN=$(echo $AWS_STS_CREDENTIALS     | jq -r .SessionToken)

    echo "AWS Temporary Access Credentials"
    echo "--------------------------------"
    echo "Named Profile         : $3"
    echo "Requested Role        : $1"
    echo "Assumed Role          : $(echo $AWS_STS_ROLE_INFO   | jq -r .AssumedRoleId)"
    echo "Assumed Role Arn      : $(echo $AWS_STS_ROLE_INFO   | jq -r .Arn)"
    echo "Expires on            : $(echo $AWS_STS_CREDENTIALS | jq -r .Expiration)"
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
    local AWS_STS_ROLE_ARN="arn:aws:iam::$AWS_ACCOUNT_NUMBER:role/$2"
    local AWS_STS_SESSION_NAME="$2-Session-$(/bin/date +%Y%m%dT%H%M%S)"

    export AWS_USER_PROFILE="$1"

    aws_get_temporary_credentials $AWS_STS_ROLE_ARN      \
                                  $AWS_STS_SESSION_NAME  \
                                  $AWS_USER_PROFILE

    if (($? > 0)); then
        return 1
    fi

    return 0
}


function load_aws_credentials()
{
    # Load user profile from ~/.aws/credentials
    local AWS_CLI_PROFILE=${1:-"$AWS_PROFILE"}

    if [ -z "$AWS_CLI_PROFILE" ]; then
        error "AWS profile not specified."
        return 1
    fi

    # Override the AWS_PROFILE if the caller specified an override
    export AWS_PROFILE=$AWS_CLI_PROFILE

    # Use profile to check for access and secret keys
    local ACCESS_KEY_ID=$(aws configure get aws_access_key_id --profile ${AWS_CLI_PROFILE} 2> /dev/null)
    if [ -z $ACCESS_KEY_ID ]; then
        error "Invalid AWS profile. Access Key ID not found in profile '${AWS_CLI_PROFILE}'."
        return 1
    fi

    local SECRET_KEY=$(aws configure get aws_secret_access_key --profile ${AWS_CLI_PROFILE} 2> /dev/null)
    if [ -z $SECRET_KEY ]; then
        error "Invalid AWS profile. Secret Access Key not found in profile '${AWS_CLI_PROFILE}'."
        return 1
    fi

    # Obtain the region information from the profile
    local REGION_NAME=$(aws configure get region --profile ${AWS_CLI_PROFILE})

    # If the profile does not define a region use the default AWS_SERVICE_REGION setting
    export AWS_SERVICE_REGION=${REGION_NAME:-"$AWS_SERVICE_REGION"}

    if [ ! -z "$AWS_IAM_ROLE" ]; then
        # Load user profile from ~/.aws/credentials to obtain temporary
        # credentials for the role specified by the environment variable
        # AWS_IAM_ROLE
        #
        echo "AWS IAM Role"
        echo "------------"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_PROFILE           : $AWS_CLI_PROFILE"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_IAM_ROLE          : $AWS_IAM_ROLE"

        local STS_ACCESS_PROFILE=$AWS_CLI_PROFILE
        local IAM_ACCESS_ROLE=$AWS_IAM_ROLE

        aws_get_access_credentials $STS_ACCESS_PROFILE $IAM_ACCESS_ROLE

    else
        # When using AWS_PROFILE, both AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
        # must be unset in the environment in order for AWS_PROFILE to be used.
        #
        # @see https://github.com/aws/aws-cli/issues/3304
        #
        unset AWS_ACCESS_KEY_ID
        unset AWS_SECRET_ACCESS_KEY

        echo "AWS IAM User"
        echo "------------"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_PROFILE           : $AWS_CLI_PROFILE"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_ACCESS_KEY_ID     : $ACCESS_KEY_ID"
        echo "AWS_SECRET_ACCESS_KEY : $SECRET_KEY"
    fi

    return $?
}
