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
    echo "Credential Profile    : $3"
    echo "Requested Role Arn    : $1"
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
    local aws_user_profile="$1"
    local aws_sts_role_name="$2"
    local aws_sts_role_arn=""
    local aws_sts_session_name="sts-session-$(/bin/date +%Y%m%dT%H%M%S)"
    #
    # Check if user specified role is an AWS IAM Role ARN
    #
    if [[ "$aws_sts_role_name" == *"arn:aws:iam"* ]]; then
        #
        # Override the AWS_ACCOUNT_NUMBER
        #
        export AWS_ACCOUNT_NUMBER=$(echo "$aws_sts_role_name" | cut -f 5 -d ':')
        #
        # Copy over the AWS IAM Role ARN
        #
        aws_sts_role_arn=$aws_sts_role_name

    else
        #
        # Generate the AWS IAM Role ARN from account number and role name
        #
        aws_sts_role_arn="arn:aws:iam::$AWS_ACCOUNT_NUMBER:role/$2"

    fi

    aws_get_temporary_credentials $aws_sts_role_arn      \
                                  $aws_sts_session_name  \
                                  $aws_user_profile

    if [ $? -ne 0 ]; then
        return 1
    fi

    return 0
}


function load_aws_credentials()
{
    #
    # Load user profile from ~/.aws/credentials
    #
    local aws_profile_name=${1:-"default"}

    if [ -z "$aws_profile_name" ]; then
        error "AWS profile not specified."
        return 1
    fi
    #
    # Override the AWS_PROFILE if the caller specified an override
    #
    export AWS_PROFILE=$aws_profile_name
    #
    # Use profile to check for access and secret keys
    #
    local access_key_id=$(aws configure get aws_access_key_id --profile ${aws_profile_name} 2> /dev/null)
    if [ -z $access_key_id ]; then
        error "Invalid AWS profile. Access Key ID not found in profile '${aws_profile_name}'."
        return 1
    fi

    local secret_key=$(aws configure get aws_secret_access_key --profile ${aws_profile_name} 2> /dev/null)
    if [ -z $secret_key ]; then
        error "Invalid AWS profile. Secret Access Key not found in profile '${aws_profile_name}'."
        return 1
    fi
    #
    # Obtain the region information from the profile
    #
    local region_name=$(aws configure get region --profile ${aws_profile_name})
    #
    # If the profile does not define a region use the default AWS_DEFAULT_REGION
    # setting. The AWS CLI and SDK environment variable AWS_DEFAULT_REGION
    # superscedes user profile settings.
    #
    export AWS_SERVICE_REGION=${region_name:-"$AWS_DEFAULT_REGION"}
    #
    # Check if user specified a role name (or full ARN), otherwise use a safe default
    #
    local aws_iam_role_name=${2:-"default"}

    if [ ! -z "$aws_iam_role_name" ]; then
        #
        # Load user profile from ~/.aws/credentials to obtain temporary
        # credentials for the role specified by the caller.
        #
        echo "AWS IAM Role"
        echo "------------"
        echo "AWS_PROFILE           : $aws_profile_name"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_IAM_ROLE          : $aws_iam_role_name"

        aws_get_access_credentials $aws_profile_name $aws_iam_role_name

    else
        #
        # When using AWS_PROFILE, both AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
        # must be unset in the environment in order for AWS_PROFILE to be used.
        #
        # @see https://github.com/aws/aws-cli/issues/3304
        #
        unset AWS_ACCESS_KEY_ID
        unset AWS_SECRET_ACCESS_KEY

        echo "AWS IAM User"
        echo "------------"
        echo "AWS_PROFILE           : $aws_profile_name"
        echo "AWS_ACCOUNT_NUMBER    : $AWS_ACCOUNT_NUMBER"
        echo "AWS_REGION            : $AWS_SERVICE_REGION"
        echo "AWS_ACCESS_KEY_ID     : $access_key_id"
        echo "AWS_SECRET_ACCESS_KEY : $secret_key"
    fi

    return $?
}
