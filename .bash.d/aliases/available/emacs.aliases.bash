#
# Emacs related aliases
#
########################################################################################

cite 'about-alias'
about-alias 'emacs abbreviations'

case $OSTYPE in
    linux*)
        alias em='emacs'
        alias en='emacs -nw'
        alias e='emacsclient -n'
        alias et='emacsclient -t'
        alias ed='emacs --daemon'
        alias E='SUDO_EDITOR=emacsclient sudo -e'
        ;;
    darwin*)
        # Open file in the current Aquamacs window
        alias openwithaquamacs="open -a /Applications/Aquamacs.app $1"
        alias aqmacs="open -a /Applications/Aquamacs.app $1"

        # Use the emacs binary bundled with Aquamacs for terminal use
        alias emacs='/Applications/Aquamacs.app/Contents/MacOS/bin/emacs'
        alias em='/Applications/Aquamacs.app/Contents/MacOS/bin/emacs'
        alias em='/Applications/Aquamacs.app/Contents/MacOS/bin/emacs -nw'
        ;;
esac
