#
# NodeJS/NPM related aliases
#
########################################################################################

cite 'about-alias'
about-alias 'NPM and Nave abbreviations'

########################################################################################
# Generic NPM shortcuts
########################################################################################

alias ni='npm install'
alias nis='npm install --save'
alias nid='npm install --save-dev'
alias nit='npm install-test'
alias nits='npm install-test --save'
alias nitd='npm install-test --save-dev'
alias nu='npm uninstall'
alias nus='npm uninstall --save'
alias nusd='npm uninstall --save-dev'
alias np='npm publish'
alias nup='npm unpublish'
alias nlk='npm link'
alias nod='npm outdated'
alias nrb='npm rebuild'
alias nud='npm update'
alias nr='npm run'
alias nls='npm list'
alias nlsg='npm list --global'

########################################################################################
# NodeJS virtualenv aliases
########################################################################################

alias nodestartenv="exec nave use"

alias nodestopenv="exec nave exit"

alias nodecheckenv="nodejs_virtualenv_check"

########################################################################################
# Grunt related aliases
########################################################################################

# Make Grunt print stack traces by default
command -v grunt > /dev/null && alias grunt="grunt --stack"
