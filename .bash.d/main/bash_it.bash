#!/usr/bin/env bash
#
# Main Bash-It Entry Point
#
# Loads the frameworks individual components in the following order:
#
#   1. `lib/composure.bash`
#   2. Remaining files in `lib`
#   3. Enabled `aliases`
#   4. Enabled `plugins`
#   5. Enabled `completions`
#   6. `themes/colors.theme.bash`
#   7. `themes/base.theme.bash`
#   8. Custom `aliases`
#   9. Custom `plugins`
#  10. Custom `completions`
#  11. Additional custom files from either `$BASH_IT/custom` or `$BASH_IT_CUSTOM`
#  12. `main/appearance.bash`, which loads the theme selected via `$BASH_IT_THEME`
#
########################################################################################

########################################################################################
# Bash-It reload alias
########################################################################################

alias reload="source ~/.bashrc"

########################################################################################
# Bash-It loader
########################################################################################

COMPOSURE_LIBRARY="${BASH_IT}/lib/composure.bash"

# Load the composure library first to enable metadata keywords
# shellcheck source=./lib/composure.bash
source $COMPOSURE_LIBRARY

# Declare metadata keywords for 'plumbing' functions
cite _about _param _example _group _author _version

BASH_IT_LIBRARIES="${BASH_IT}/lib/*.bash"

# Load remaining library files
for config_file in $BASH_IT_LIBRARIES; do
    if [ $config_file != $COMPOSURE_LIBRARY ]; then
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

BASH_IT_CUSTOM_FILES="${BASH_IT_CUSTOM:=${BASH_IT}/custom}/**/*.bash"
for config_file in $BASH_IT_CUSTOM_FILES; do
    if [ -e "${config_file}" ]; then
        # shellcheck disable=SC1090
        source $config_file
    fi
done

unset config_file

########################################################################################
# Load appearance (themes), after all other dependencies
########################################################################################

# shellcheck source=./main/appearance.bash
source "${BASH_IT}/main/appearance.bash"

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
