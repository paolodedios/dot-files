cite about-plugin
about-plugin 'HSTR configuration

#
# Configure HSTR if it is installed.
#
# @see https://github.com/dvorka/hstr/
# @see https://github.com/dvorka/hstr/blob/master/CONFIGURATION.md
#
if [ $(type -p hstr) ]; then

    # Use 'hh' as an alias for hstr
    alias hh=hstr

    # Use hicolor mode on the ncurses UI
    export HSTR_CONFIG=hicolor

    # Leading space hides commands from history
    export HISTCONTROL=ignorespace

    # Ensure synchronization between Bash memory and history file
    export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"

    # if this is interactive shell, then bind hstr to Ctrl-r (for Vi mode check doc)
    if [[ $- =~ .*i.* ]]; then
        bind '"\C-r": "\C-a hstr -- \C-j"';
    fi

    # if this is interactive shell, then bind 'kill last command' to Ctrl-x k
    if [[ $- =~ .*i.* ]]; then
        bind '"\C-xk": "\C-a hstr -k \C-j"';
    fi
fi
