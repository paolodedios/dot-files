#
# macOS Aliases
#
########################################################################################

cite 'about-alias'
about-alias 'macOS specific abbreviations'

case $OSTYPE in
    darwin*)
        # Launch common desktop programs from the command line
        alias preview="open -a '$PREVIEW'"
        alias f='open -a Finder '
        alias fh='open -a Finder .'
        alias textedit='open -a TextEdit'
        alias hex='open -a "Hex Fiend"'
        alias subl='open -a Sublime\ Text'

        alias xcode="open -a '/Applications/XCode.app'"
        alias safari="open -a safari"
        alias firefox="open -a firefox"
        alias chrome="open -a google\ chrome"
        alias chromium="open -a chromium"

        # Macports /opt/local/bin/gshred should be the same as linus shred
        alias shred="gshred"

        # Public ip check
        alias publicip="curl -s http://checkip.dyndns.com/ | sed 's/[^0-9\.]//g'"
        alias localip="ipconfig getifaddr $NETIF"

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

        # Clean up LaunchServices to remove duplicates in the “Open With” menu
        alias rebuild-menu="$LS_REGISTER_PATH/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

        # Reboot the Finder
        alias kill-finder="killall Finder && open /System/Library/CoreServices/Finder.app"

        # Reboot the window server/manager and force a logout
        alias kill-windowserver="sudo killall -HUP WindowServer"

        # Cleanup Resource Forks
        alias cleanresforks="find . -name \*._*|xargs \rm"

        # Flush Directory Service cache
        alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

        # ROT-13-encode text. Works for decoding also
        alias rot13="tr a-zA-Z n-za-mN-ZA-M"

        # Hide/show all desktop icons (useful when presenting)
        alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
        alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

        # Disable Spotlight
        alias spotoff="sudo mdutil -a -i off"

        # Enable Spotlight
        alias spoton="sudo mdutil -a -i on"

        # Show program names with lsof
        alias slsof="sudo lsof -i -P"

        # Canonical hex dump; some systems have this symlinked
        command -v hd > /dev/null || alias hd="hexdump -C"

        # OS X has no `md5sum`, so use `md5` as a fallback
        command -v md5sum > /dev/null || alias md5sum="md5"

        # OS X has no `sha1sum`, so use `shasum` as a fallback
        command -v sha1sum > /dev/null || alias sha1sum="shasum"

        # If Matlab is installed, ensure it runs properly on the command line
        command -v matlab > /dev/null && alias matlab="matlab_console"
        command -v matlab > /dev/null && alias matlabex="matlab_run_file"
        ;;
esac
