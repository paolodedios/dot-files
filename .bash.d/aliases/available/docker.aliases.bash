#
# Docker Aliases
#
########################################################################################

cite 'about-alias'
about-alias 'docker command abbreviations'

########################################################################################
# Docker specific shortcuts
########################################################################################

# List last Docker container
alias dklc='docker ps -l'

# List last Docker container ID
alias dklcid='docker ps -l -q'

# Tail logs from docker container
alias dklogs='docker logs -f'

# Get IP of last Docker container
alias dklcip='docker inspect -f "{{.NetworkSettings.IPAddress}}" $(docker ps -l -q)'

# List running Docker containers
alias dkps='docker ps'

# List all Docker containers
alias dkpsa='docker ps -a'

# List Docker images
alias dki='docker images'

# Delete all Docker containers
alias dkrmac='docker rm $(docker ps -a -q)'

# Delete most recent (i.e., last) Docker container
alias dkrmlc='docker-remove-most-recent-container'

# Delete all untagged Docker images
case $OSTYPE in
    darwin*|*bsd*|*BSD*)
        alias dkrmui='docker images -q -f dangling=true | xargs docker rmi'
        ;;
    *)
        alias dkrmui='docker images -q -f dangling=true | xargs -r docker rmi'
        ;;
esac

# Delete all untagged images and exited containers
alias dkrmall='docker-remove-stale-assets'

# Delete most recent (i.e., last) Docker image
alias dkrmli='docker-remove-most-recent-image'

# Delete images for supplied IDs or all if no IDs are passed as arguments
alias dkrmi='docker-remove-images'

# Output a graph of image dependencies using Graphiz
alias dkideps='docker-image-dependencies'

# List environmental variables of the supplied image ID
alias dkre='docker-runtime-environment'

# Enter last container (works with Docker 1.3 and above)
alias dkelc='docker exec -it `dklcid` bash'

########################################################################################
# Docker Compose specific shortcuts
########################################################################################

# Docker compose shortcut
alias dco="docker-compose"

# Refresh setup
alias dcofresh="docker-compose-fresh"

# Tail docker compose logs
alias dcol="docker-compose logs -f --tail 100"
