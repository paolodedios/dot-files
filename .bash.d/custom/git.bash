#!/bin/bash
#
# Git helper functions
#
########################################################################################

# Take repo in $pwd and copy it to the specified location, minus the .git specific files.
function git_export()
{
    mkdir -p "$1"
    git archive master | tar -x -C "$1"
}

# Designate specified git repo URL as the upstream or central repo
function git_add_upstream()
{
    git remote add upstream "$1"
}

# Pull the latest changes from the upstream or central repo
function git_pull_upstream()
{
    git pull upstream master "$1"
}

# Push an untracked local branch to the remote origin
function git_push_new_branch()
{
    git push --set-upstream origin $(git_branch_name)
}

# Delete local and remote branches
function git_delete_branch()
{
    echo "Deleting local and remote tracking branch for: ${1}"
    git push origin --delete "$1"
    git branch -d "$1"
}

# Sync local branches with upstream
function git_prune_branches()
{
    git fetch --all --prune
}

# Reset last commit. If last commit was pushed upstream then a
# git pull may be necessary before re-committing and pushing
# changes
function git_undo_last_commit()
{
    git reset HEAD~
}

# Discard all local changes in the current branch
function git_reset_branch()
{
    git checkout -- .
}

# Discard all local changes to a file
function git_discard_changes()
{
    git checkout -- "$1"
}

# Use Git's colored diff when available
hash git &>/dev/null
if [ $? -eq 0 ]; then
    function diff() {
        git diff --no-index --color-words "$@"
    }
fi

########################################################################################
# Git helper function aliases
########################################################################################

alias gitaddup="git_add_upstream"
alias gitpullup="git_pull_upstream"
alias gitreset="git_reset_branch"
alias gitdiscard="git_discard_changes"
