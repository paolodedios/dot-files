#!/bin/bash
#
# Golang environment variables
#
########################################################################################

# Declare personal projects directory
if [ -d $LOCAL_PROJECTS ]; then
    export GO_PROJECT_HOME=$LOCAL_PROJECTS
else
    export GO_PROJECT_HOME=$HOME
fi

# Set the top level GOPATH environment variable
# @see https://golang.org/doc/code.html#Workspaces
#
export GOPATH=$GO_PROJECT_HOME/Go
export PATH=$GOPATH/bin:$PATH
