#!/usr/bin/env bash
#
########################################################################################

if [[ $BASH_PREVIEW ]]; then
    # Prevent infinite looping
    unset BASH_PREVIEW
    echo "

  Previewing Bash-it Themes
  -------------------------
    "

    THEMES="$BASH_IT/themes/*/*.theme.bash"
    for theme in $THEMES; do
        BASH_IT_THEME=${theme%.theme.bash}
        BASH_IT_THEME=${BASH_IT_THEME##*/}
        echo "
    $BASH_IT_THEME"
        echo "" | bash --init-file "${BASH_IT}/main/bash_it.bash" -i
    done
fi
