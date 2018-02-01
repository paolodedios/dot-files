#
# Linux systemd aliases
#
########################################################################################

cite 'about-alias'
about-alias 'Linux systemd abbreviations'

case $OSTYPE in
    linux*)
        alias scstat='systemctl status'
	    alias sc='systemctl'
	    alias scr='systemctl daemon-reload'
	    alias scu='systemctl --user'
	    alias scur='systemctl --user daemon-reload'
	    alias sce='systemctl stop'
	    alias scue='systemctl --user stop'
	    alias scs='systemctl start'
	    alias scus='systemctl --user start'
    ;;
esac
