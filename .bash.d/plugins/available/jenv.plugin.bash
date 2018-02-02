cite about-plugin
about-plugin 'load jenv, if you are using it'

export JENV_ROOT="$HOME/.jenv"
pathmunge "$JENV_ROOT/bin"

if [ $(type -p jenv) ]; then
    eval "$(jenv init -)";
fi

[[ -e $JENV_ROOT/completions/jenv.bash ]] && source $JENV_ROOT/completions/jenv.bash
