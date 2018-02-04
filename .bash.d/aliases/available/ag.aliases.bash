#
# Summary for args to less:
# less(1)
#   -M (-M or --LONG-PROMPT) Prompt very verbosely
#   -I (-I or --IGNORE-CASE) Searches with '/' ignore case
#   -R (-R or --RAW-CONTROL-CHARS) For handling ANSI colors
#   -F (-F or --quit-if-one-screen) Auto exit if <1 screen
#   -X (-X or --no-init) Disable termcap init & deinit
#
########################################################################################

cite 'about-alias'
about-alias 'the silver searcher (ag) abbreviations'

alias ag='ag --smart-case --pager="less -MIRFX"'

case $OSTYPE in
    linux*)
        #
        # If the 'ack' command is not found on the PATH then alias
        # the ack command to the silver searcher
        #
        if [ ! $(type -p ack) ]; then
            alias ack="ag"
        fi
    ;;
esac
