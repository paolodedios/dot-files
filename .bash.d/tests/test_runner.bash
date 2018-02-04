#!/usr/bin/env bash
#
########################################################################################

test_directory="$(cd "$(dirname "$0")" && pwd)"
bats_executable="${test_directory}/../lib/bats/bats-core/bin/bats"

if [ -z "${BASH_IT}" ]; then
    declare BASH_IT
    BASH_IT=$(cd ${test_directory} && dirname "$(pwd)")
    export BASH_IT
fi

if [ -z "$1" ]; then
    test_dirs=( ${test_directory}/{bash_it,completion,lib,plugins,themes} )
else
    test_dirs=( "$1" )
fi

exec $bats_executable ${CI:+--tap} "${test_dirs[@]}"
