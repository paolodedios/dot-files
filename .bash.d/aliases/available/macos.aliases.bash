#
# macOS Aliases
#
########################################################################################

cite 'about-alias'
about-alias 'macOS specific abbreviations'

case $OSTYPE in
    darwin*)
        # Desktop Programs
        alias preview="open -a '$PREVIEW'"
        alias xcode="open -a '/Applications/XCode.app'"
        alias safari="open -a safari"
        alias firefox="open -a firefox"
        alias chrome="open -a google\ chrome"
        alias chromium="open -a chromium"
        alias f='open -a Finder '
        alias fh='open -a Finder .'
        alias textedit='open -a TextEdit'
        alias hex='open -a "Hex Fiend"'
        alias subl='open -a Sublime\ Text'

        # Requires growlnotify, which can be found in the Growl DMG under "Extras"
        alias grnot='growlnotify -s -t Terminal -m "Done"'

        # Get rid of those pesky .DS_Store files recursively
        alias dsclean='find . -type f -name .DS_Store -delete'

        # Track who is listening to your iTunes music
        alias whotunes='lsof -r 2 -n -P -F n -c iTunes -a -i TCP@`hostname`:3689'

        # Flush your dns cache
        alias flush='dscacheutil -flushcache'

        # From http://apple.stackexchange.com/questions/110343/copy-last-command-in-terminal
        alias copyLastCmd='fc -ln -1 | awk '\''{$1=$1}1'\'' ORS='\'''\'' | pbcopy'

        # Use Finder's Quick Look on a file (^C or space to close)
        alias ql='qlmanage -p 2>/dev/null'

        # Mute/Unmute the system volume. Plays nice with all other volume settings.
        alias mute="osascript -e 'set volume output muted true'"
        alias unmute="osascript -e 'set volume output muted false'"

        # Pin to the tail of long commands for an audible alert after long processes
        # curl http://downloads.com/hugefile.zip; lmk
        alias lmk="say 'Process complete.'"
        ;;
esac
