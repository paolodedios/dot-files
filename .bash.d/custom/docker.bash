#!/bin/bash
#
# Helper functions for simple administration of local docker repositories
# and repositories on AWS ECR.
#
################################################################################

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
# AWS tooling checks
#
################################################################################

function ensure_cli_tools()
{
    if [ ! $(type -p jq) ]; then
        error "Missing jq JSON processor. Please install."
        return 1
    fi
}

function ensure_aws_cli_tools()
{
    if [ ! $(type -p aws) ]; then
        error "Missing AWS CLI tools. Please install via:"
        error " $ pip install awscli"
        return 1
    fi
}


function ensure_aws_sts_credentials()
{
    if [ -d $HOME/.aws ]; then
        notice "Using AWS ECR credentials : $HOME/.aws/credentials"
        notice "Using AWS ECR config      : $HOME/.aws/config"
        echo ""
    else
        error "Missing AWS credentials and config file."
        echo ""
        return 1
    fi

    return 0
}


function verify_aws_setup()
{
    if ! ensure_cli_tools; then
        return 1
    fi

    if ! ensure_aws_cli_tools; then
        return 1
    fi

    if ! ensure_aws_sts_credentials; then
        return 1
    fi

    return 0
}

################################################################################
#
# AWS ECR helpers
#
################################################################################

function list_images_on_aws_ecr()
{
    aws ecr list-images --region $AWS_SERVICE_REGION --repository-name $1
}


function batch_delete_untagged_aws_ecr_images()
{
    local ECR_IMAGE_LIST=$(aws ecr list-images --repository-name $1)

    # Generate AWS CLI shorthand syntax. Currently not supported since
    # the image-ids list is a nested structure consisting of a list
    # of objects where they object key is either imageDigest or imageTag
    #
    # @see http://docs.aws.amazon.com/cli/latest/userguide/shorthand-syntax.html
    #
    # For information on the cross platform sed invocation,
    # @see http://stackoverflow.com/questions/1251999/how-can-i-replace-a-newline-n-using-sed
    #
    # local ECR_UNTAGGED_IMAGES=$(echo $ECR_IMAGE_LIST |
    #                            jq '.imageIds | .[] | select(.imageTag==null) | .imageDigest' |
    #                            sed -e 's/\"//g' |
    #                            sed -e 's/sha256/imageDigest=sha256/g' |
    #                            sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/,/g' )

    # Generate AWS CLI JSON syntax
    #
    # 1. Parse ECR_IMAGE_LIST result set via JQ
    # 2. Execute JQ filter that returns list of elements in the imageId structure
    #    and select all results where the imageTag attribute is null.
    # 3. Collapse newlines from final JQ result by replacing newlines with a space.
    # 4. Insert comma between result boundaries '} {' => '}, {'
    #
    local ECR_UNTAGGED_IMAGES=$(echo $ECR_IMAGE_LIST |
                                jq '.imageIds | .[] | select(.imageTag==null)' |
                                sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g' |
                                sed -e 's/} {/}, {/g' )

    if [ ! -z "$ECR_UNTAGGED_IMAGES" ]; then

        echo "Found untagged images to delete : "

        echo $ECR_IMAGE_LIST | jq '.imageIds | .[] | select(.imageTag==null)'
        echo

        aws ecr batch-delete-image --region $AWS_SERVICE_REGION --repository-name $1 --image-ids "[ $ECR_UNTAGGED_IMAGES ]"

        echo
        echo "Remaining images for repository $1"

        aws ecr list-images --region $AWS_SERVICE_REGION --repository-name $1

    else
        echo "No untagged images found."
    fi
}


function batch_regex_delete_aws_ecr_images()
{
    local ECR_IMAGE_LIST=$(aws ecr list-images --region $AWS_SERVICE_REGION --repository-name $1)

    # Generate AWS CLI JSON syntax
    #
    # 1. Parse ECR_IMAGE_LIST result set via JQ
    # 2. Execute JQ filter that returns list of elements in the imageId structure
    #    and for each result item pass the value of the imageTag attribute to
    #    the select test filter. Then select the imageTag attribute if it passes
    #    the user defined regular expression test and interpolate value into a
    #    single attribute ("imageTag") JSON structure.
    # 3. Remove double quotes from the result string
    # 4. Replace single quotes with double quotes for legal JSON
    # 5. Collapse newlines from final JQ result by replacing newlines with a space.
    # 6. Insert comma between result boundaries '} {' => '}, {'
    #
    local ECR_TAGGED_IMAGES=$(echo $ECR_IMAGE_LIST |
                              jq ".imageIds | .[] | .imageTag | select(test(\"$2\")) | \"{ 'imageTag' : '\(.)' }\"" |
                              sed -e 's/"//g' |
                              sed -e "s/'/\"/g" |
                              sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g' |
                              sed -e 's/} {/}, {/g' )

    if [ ! -z "$ECR_TAGGED_IMAGES" ]; then

        echo "Found tagged images to delete : "

        echo $ECR_TAGGED_IMAGES
        echo

        aws ecr batch-delete-image --region $AWS_SERVICE_REGION --repository-name $1 --image-ids "[ $ECR_TAGGED_IMAGES ]"

        echo
        echo "Remaining images for repository $1"

        aws ecr list-images --region $AWS_SERVICE_REGION --repository-name $1

    else
        echo "No tagged images found."
    fi
}


function delete_aws_ecr_image()
{
    echo "Deleting image from AWS ECR :  $1:$2"
    echo

    aws ecr batch-delete-image --region $AWS_SERVICE_REGION --repository-name $1 --image-ids imageTag=$2

    echo
    echo "Remaining images for repository $1"

    aws ecr list-images --region $AWS_SERVICE_REGION --repository-name $1
}


function push_image_to_aws_ecr()
{
    # Use the AWS CLI tool to obtain AWS EC2 ECR docker login credentials that
    # produces an authorization token that is valid for 12 hours.
    #
    # The following command assumes that the proper default AWS credentials
    # are specified in ~/.aws/credentials
    local AWS_EC2_ECR_DOCKER_LOGIN=$(aws ecr get-login --region $AWS_SERVICE_REGION --no-include-email)

    if [ -z "$AWS_EC2_ECR_DOCKER_LOGIN" ]; then
        error "'aws ecr get-login' returned no results."
        return 1
    fi

    # Extract the registry hostname from the url
    local REGISTRY_URL=$(echo $AWS_EC2_ECR_DOCKER_LOGIN | awk '{ print $7 }')
    local PROTOCOL_STRING=$(echo $REGISTRY_URL | grep :// | sed -e's,^\(.*://\).*,\1,g')

    export AWS_EC2_ECR_DOCKER_REGISTRY_HOST=$(echo ${REGISTRY_URL/$PROTOCOL_STRING/})

    echo "Using AWS Elastic Container Registry : $REGISTRY_URL"

    eval $AWS_EC2_ECR_DOCKER_LOGIN

    # Tag the image with a fully qualified repository
    docker tag $1 $AWS_EC2_ECR_DOCKER_REGISTRY_HOST/$1

    # Push image to ECR
    docker push $AWS_EC2_ECR_DOCKER_REGISTRY_HOST/$1

    return 0
}


function pull_image_from_aws_ecr()
{
    # Use the AWS CLI tool to obtain AWS EC2 ECR docker login credentials that
    # produces an authorization token that is valid for 12 hours.
    #
    # The following command assumes that the proper default AWS credentials
    # are specified in ~/.aws/credentials
    local AWS_EC2_ECR_DOCKER_LOGIN=$(aws ecr get-login --region $AWS_SERVICE_REGION --no-include-email)

    if [ -z "$AWS_EC2_ECR_DOCKER_LOGIN" ]; then
        error "'aws ecr get-login' returned no results."
        return 1
    fi

    # Extract the registry hostname from the url
    local REGISTRY_URL=$(echo $AWS_EC2_ECR_DOCKER_LOGIN | awk '{ print $7 }')
    local PROTOCOL_STRING=$(echo $REGISTRY_URL | grep :// | sed -e's,^\(.*://\).*,\1,g')

    export AWS_EC2_ECR_DOCKER_REGISTRY_HOST=$(echo ${REGISTRY_URL/$PROTOCOL_STRING/})

    echo "Using AWS Elastic Container Registry : $REGISTRY_URL"

    eval $AWS_EC2_ECR_DOCKER_LOGIN

    # Pull image from ECR
    docker pull $AWS_EC2_ECR_DOCKER_REGISTRY_HOST/$1

    return 0
}


function create_ecr_repository()
{
    echo
    echo "Creating repository : ${1}"

    aws ecr create-repository --region $AWS_SERVICE_REGION --repository-name $1

    if (($? > 0)); then
        error "Failed to create repository : ${1}"
        return 1
    fi

    return 0
}


function set_ecr_repository_policy()
{
    echo
    echo "Using policy text   :"
    echo "$(cat $2)"

    if [ ! -f $2 ]; then
        error "Repository policy file not found : ${2}"
        return 1
    fi

    echo "Setting repository policy for ${1}"

    aws ecr set-repository-policy --region $AWS_SERVICE_REGION          \
                                  --registry-id $AWS_ACCOUNT_NUMBER     \
                                  --repository-name $1                  \
                                  --policy-text file://$2


    if (($? > 0)); then
        error "Failed to set repository policy with ${2}"
        return 1
    fi

    return 0
}


function put_ecr_lifecycle_policy()
{
    echo
    echo "Using policy text   :"
    echo "$(cat $2)"

    if [ ! -f $2 ]; then
        error "Lifecycle policy file not found : ${2}"
        return 1
    fi

    echo "Putting lifecycle policy for ${1}"

    aws ecr put-lifecycle-policy --region $AWS_SERVICE_REGION          \
                                 --registry-id $AWS_ACCOUNT_NUMBER     \
                                 --repository-name $1                  \
                                 --lifecycle-policy-text file://$2


    if (($? > 0)); then
        error "Failed to put lifecycle policy with ${2}"
        return 1
    fi

    return 0
}

################################################################################
#
# 'dockerh' : main entry point for the docker helper
#
################################################################################

function dockerh()
{
    case "$1" in
        new-from-dockerfile)
            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh new-from-dockerfile <repo/image:tag> <PATH | URL>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then
                docker build --no-cache -t $2 $3

            else
                error "Dockerfile params not specified correctly."
            fi

            ;;

        volumes)
            if [ ! -z $2 ]; then
                docker volume ls | grep $2
            else
                docker volume ls
            fi

            ;;

        delete-orphaned-volumes)
            DANGLING_VOLUMES=$(docker volume ls --filter "dangling=true" -q)

            if [ ! -z "$DANGLING_VOLUMES" ]; then
                docker volume rm $(docker volume ls --filter "dangling=true" -q)

                docker volume ls
            else
                error "No orphaned volumes found."
            fi

            ;;

        images)
            if [ ! -z $2 ]; then
                docker images | grep $2
            else
                docker images
            fi
            ;;

        delete-untagged-images)
            DANGLING_IMAGES=$(docker images --filter "dangling=true" -q --no-trunc)

            if [ ! -z "$DANGLING_IMAGES" ]; then
                docker rmi -f $(docker images --filter "dangling=true" -q --no-trunc)

                docker images
            else
                error "No untagged images found in the local repository."
            fi
            ;;

        ps)
            if [ ! -z $2 ]; then
                docker ps -a | grep $2
            else
                docker ps -a
            fi
            ;;

        remove-stopped-containers)
            STOPPED_CONTAINERS=$(docker ps -a -q)

            if [ ! -z "$STOPPED_CONTAINERS" ]; then
                # Remove stopped containers and associated links
                docker rm -f $STOPPED_CONTAINERS
            else
                error "No stopped containers found."
            fi
            ;;

        remove-all)
            RUNNING_CONTAINERS=$(docker ps -q)
            if [ ! -z "$RUNNING_CONTAINERS" ]; then
                # Stop all running containers
                echo "Stop all running containers?"
                docker stop $(docker ps -q)
            fi

            STOPPED_CONTAINERS=$(docker ps -a -q)

            if [ ! -z "$STOPPED_CONTAINERS" ]; then
                # Remove stopped containers and associated links
                echo "Remove all stopped containers?"
                docker rm -f $STOPPED_CONTAINERS
            fi

            ALL_IMAGES=$(docker images -q)
            if [ ! -z "$ALL_IMAGES" ]; then
                # Remove stopped containers and associated links
                echo "Remove all cached images?"
                docker rmi -f $ALL_IMAGES
            fi
            ;;

        load-ecr-credentials)
            if ! verify_aws_setup; then
                return 1
            fi

            if ! load_aws_credentials $2; then
                return 1
            fi
            ;;

        create-ecr-repository)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh create-ecr-repository <aws_profile> <repository_name>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                create_ecr_repository $3

            else
                error "AWS profile, repository and policy file not specified correctly."
            fi
            ;;

        set-ecr-repository-policy)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 4 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh set-ecr-repository-policy <aws_profile> <repository_name> <policy-file-path>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 -a -n $4 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                set_ecr_repository_policy $3 $4

            else
                error "AWS profile, repository and policy file not specified correctly."
            fi
            ;;

        put-ecr-lifecycle-policy)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 4 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh put-ecr-lifecycle-policy <aws_profile> <repository_name> <policy-file-path>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 -a -n $4 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                put_ecr_lifecycle_policy $3 $4

            else
                error "AWS profile, repository and policy file not specified correctly."
            fi
            ;;

        list-ecr-images)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "ERROR. Wrong number of parameters specified"
                notice "Usage: dockerh list-ecr-images <aws_profile> <repository_name>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                list_images_on_aws_ecr $3
            else
                error "AWS profile and docker repository name not specified correctly."
            fi
            ;;

        delete-untagged-ecr-images)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh delete-untagged-ecr-images <aws_profile> <repository_name>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                batch_delete_untagged_aws_ecr_images $3
            else
                error "ERROR. AWS profile and docker repository name not specified correctly."
            fi
            ;;

        regex-delete-ecr-images)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 4 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh regex-delete-ecr-images <aws_profile> <repository_name> <regex_pattern>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 -a -n $4 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                batch_regex_delete_aws_ecr_images $3 $4
            else
                error "AWS profile and docker repository name not specified correctly."
            fi
            ;;

        delete-ecr-image)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 4 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh delete-ecr-image <aws_profile> <repository_name> <image_tag>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 -a -n $4 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                delete_aws_ecr_image $3 $4
            else
                error "AWS profile and docker repository name not specified correctly."
            fi
            ;;

        pull-public-image)
            if [ ! -z $2 ]; then

                docker logout 2>&1 > /dev/null

                docker pull $2

                docker images
            else
                error "Docker public image not specified."
            fi
            ;;

        push-image-to-ecr)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh push-image-to-ecr <aws_profile> <repository_name>:<tag>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                push_image_to_aws_ecr $3
            else
                error "AWS profile and docker image not specified correctly."
            fi
            ;;

        pull-image-from-ecr)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh pull-image-from-ecr <aws_profile> <repository_name>:<tag>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                if ! load_aws_credentials $2; then
                    return 1
                fi

                pull_image_from_aws_ecr $3
            else
                error "AWS profile and docker image not specified correctly."
            fi
            ;;

        clone-to-ecr)
            if ! verify_aws_setup; then
                return 1
            fi

            if [ "$#" -ne 3 ]; then
                error  "Wrong number of parameters specified"
                notice "Usage: dockerh clone-to-ecr <aws_profile> <repository_name>:<tag>"
                echo
                return 1
            fi

            if [ -n $2 -a -n $3 ]; then

                docker logout 2>&1 > /dev/null

                docker pull $3

                docker images

                if ! load_aws_credentials $2; then
                    return 1
                fi

                push_image_to_aws_ecr $3
            else
                error "AWS profile and docker image not specified correctly."
            fi
            ;;

        gc)

            echo

            # Remove dangling images manually since docker-gc does not
            # do a good job of reaping them.
            docker rmi -f $(docker images --filter "dangling=true" -q --no-trunc) > /dev/null 2>&1

            docker run --rm                                     \
                   -v /var/run/docker.sock:/var/run/docker.sock \
                   -v /etc:/etc:ro                              \
                   --env "MINIMUM_IMAGES_TO_SAVE=2"             \
                   --env "GRACE_PERIOD_SECONDS=21600"           \
                   spotify/docker-gc

            echo "Remaining Docker images:"
            docker images
            ;;

        gc-force)

            echo

            # Remove dangling images manually since docker-gc does not
            # do a good job of reaping them.
            docker rmi -f $(docker images --filter "dangling=true" -q --no-trunc) > /dev/null 2>&1

            docker run --rm                                     \
                   -v /var/run/docker.sock:/var/run/docker.sock \
                   -v /etc:/etc:ro                              \
                   --env "MINIMUM_IMAGES_TO_SAVE=0"             \
                   --env "FORCE_IMAGE_REMOVAL=1"                \
                   --env "FORCE_CONTAINER_REMOVAL=1"            \
                   --env "GRACE_PERIOD_SECONDS=3600"            \
                   spotify/docker-gc

            echo "Remaining Docker images:"
            docker images
            ;;

        showvars)
            echo "dockerh environment settings"
            echo "----------------------------"
            printenv | sort -df | grep "AWS"
            ;;

        help|*)
            echo "Usage: dockerh {command} option1 option2 ..."
            echo "  Commands                        | Description "
            echo "  -----------------------------     ----------- "
            echo "  new-from-dockerfile             : Build docker image from Dockerfile specification"
            echo "  volumes                         : List local data volumes"
            echo "  delete-orphaned-volumes         : Delete local data volumes not referenced by any cointainers"
            echo "  images                          : List local images for a given repository"
            echo "  delete-untagged-images          : Delete untagged images from the local machine"
            echo "  ps                              : List all containers [running/stopped/paused]"
            echo "  remove-stopped-containers       : Remove all stopped containers"
            echo "  remove-all                      : Remove all containers, images, and cache"
            echo "  load-ecr-credentials            : Get AWS ECR Credentials"
            echo "  create-ecr-repository           : Create new ECR repository"
            echo "  set-ecr-repository-policy       : Set/Update ECR repository policy"
            echo "  put-ecr-lifecycle-policy        : Add ECR lifecycle policy"
            echo "  list-ecr-images                 : List AWS ECR images for a given repository"
            echo "  delete-untagged-ecr-images      : Delete untagged AWS ECR images for a given repository"
            echo "  regex-delete-ecr-images         : Delete tagged AWS ECR images for a given repository using a PCRE regex"
            echo "  delete-ecr-image                : Delete AWS ECR image by tag for a given repository"
            echo "  pull-public-image               : Pull public image from Docker Hub"
            echo "  push-image-to-ecr               : Push local image to AWS ECR"
            echo "  pull-image-from-ecr             : Pull private image from AWS ECR and tag with canonical name"
            echo "  clone-to-ecr                    : Pull public image from Docker Hub and push to AWS CR"
            echo "  gc                              : Launch spotify/docker-gc container to cleanup local image repository"
            echo "  gc-force                        : Force remove images and containers from local image repository"
            echo "  showvars                        : Display applicable environment variables"
            ;;
    esac

    return 0
}
