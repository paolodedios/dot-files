#!/usr/bin/env bash
#
# kubectx completion
#
# @see https://github.com/ahmetb/kubectx
#
########################################################################################

_kube_contexts()
{
    local curr_arg;
    curr_arg=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "- $(kubectl config get-contexts --output='name')" -- $curr_arg ) );
}

_kube_namespaces()
{
    local curr_arg;
    curr_arg=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "- $(kubectl get namespaces -o=jsonpath='{range .items[*].metadata.name}{@}{"\n"}{end}')" -- $curr_arg ) );
}

#
# Enable completions if kubectx and kubens are available
#
if command -v kubectx &>/dev/null
then
    complete -F _kube_contexts kubectx kctx
fi


if command -v kubens &>/dev/null
then
    complete -F _kube_namespaces kubens kns
fi
