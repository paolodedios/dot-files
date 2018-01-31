#!/usr/bin/env bash
#
# Main Bash-It Entry Point
#
########################################################################################

########################################################################################
# Bash-It reload alias
########################################################################################

alias reload="source ~/.bashrc"

########################################################################################
# Bash-It loader
########################################################################################

# Load composure first, so we support function metadata

# shellcheck source=./lib/composure.bash
source "${BASH_IT}/lib/composure.bash"

# support 'plumbing' metadata
cite _about _param _example _group _author _version

# libraries, but skip appearance (themes) for now
LIBRARIES="${BASH_IT}/lib/*.bash"
APPEARANCE_LIBRARY="${BASH_IT}/lib/appearance.bash"

for config_file in $LIBRARIES; do
  if [ $config_file != $APPEARANCE_LIBRARY ]; then
    # shellcheck disable=SC1090
    source $config_file
  fi
done

########################################################################################
# Load the global "enabled" directory
########################################################################################

_load_global_bash_it_files

########################################################################################
# Load enabled aliases, completion, plugins
########################################################################################

for file_type in "aliases" "plugins" "completion"; do
    _load_bash_it_files $file_type
done

########################################################################################
# Load colors and helpers first so they can be used in base theme
########################################################################################

# shellcheck source=./themes/colors.theme.bash
source "${BASH_IT}/themes/colors.theme.bash"

# shellcheck source=./themes/githelpers.theme.bash
source "${BASH_IT}/themes/githelpers.theme.bash"

# shellcheck source=./themes/base.theme.bash
source "${BASH_IT}/themes/base.theme.bash"

########################################################################################
# Load appearance (themes) now, after all dependencies
########################################################################################

# shellcheck source=./lib/appearance.bash
source $APPEARANCE_LIBRARY

########################################################################################
# Load custom aliases, completion, plugins
########################################################################################

for file_type in "aliases" "completion" "plugins"; do
    if [ -e "${BASH_IT}/${file_type}/custom.${file_type}.bash" ]; then
        # shellcheck disable=SC1090
        source "${BASH_IT}/${file_type}/custom.${file_type}.bash"
    fi
done

########################################################################################
# Load global custom configurations and overrides
########################################################################################

CUSTOM_FILES="${BASH_IT_CUSTOM:=${BASH_IT}/custom}/*.bash ${BASH_IT_CUSTOM:=${BASH_IT}/custom}/**/*.bash"
for config_file in $CUSTOM_FILES; do
    if [ -e "${config_file}" ]; then
        # shellcheck disable=SC1090
        source $config_file
    fi
done

unset config_file

########################################################################################
# Set override for PS1 prompt via PROMPT environment variable
########################################################################################

if [[ $PROMPT ]]; then
    export PS1="\[""$PROMPT""\]"
fi

########################################################################################
# Declare global binary for PREVIEW
########################################################################################

if [ -s /usr/bin/gloobus-preview ]; then
    PREVIEW="gloobus-preview"
elif [ -s /Applications/Preview.app ]; then
    # shellcheck disable=SC2034
    PREVIEW="/Applications/Preview.app"
else
    PREVIEW="less"
fi

########################################################################################
# Disable trap DEBUG on subshells - https://github.com/Bash-it/bash-it/pull/1040
########################################################################################

set +T
