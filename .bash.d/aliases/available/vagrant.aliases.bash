#
# Vagrant aliases
#
########################################################################################

cite 'about-alias'
about-alias 'Hashicorp Vagrant abbreviations'

########################################################################################
# Basic command aliases
########################################################################################

alias vhl='vagrant hosts list'
alias vscp='vagrant scp'
alias vsl='vagrant snapshot list'
alias vst='vagrant snapshot take'
alias vup="vagrant up"
alias vupl="vagrant up 2>&1 | tee vagrant.log"
alias vh="vagrant halt"
alias vs="vagrant suspend"
alias vr="vagrant resume"
alias vrl="vagrant reload"
alias vssh="vagrant ssh"
alias vst="vagrant status"
alias vp="vagrant provision"
alias vdstr="vagrant destroy"

########################################################################################
# Vagrant plugin aliases
########################################################################################

# Requires vagrant-list plugin
alias vl="vagrant list"

# Requires vagrant-hostmanager plugin
alias vhst="vagrant hostmanager"
