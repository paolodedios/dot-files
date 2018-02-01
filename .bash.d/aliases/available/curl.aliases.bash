#
# cURL Aliases
#
########################################################################################

cite 'about-alias'
about-alias 'Common cURL command/options abbreviations'

# follow redirects
alias cl='curl -L'

# follow redirects, download as original name
alias clo='curl -L -O'

# follow redirects, download as original name, continue
alias cloc='curl -L -C - -O'

# follow redirects, download as original name, continue, retry 5 times
alias clocr='curl -L -C - -O --retry 5'

# follow redirects, fetch banner
alias clb='curl -L -I'

# see only response headers from a get request
alias clhead='curl -D - -so /dev/null'

# Gzip-enabled `curl`, follow redirects, download as original name, continue
alias gurl='curl --compressed -L -C - -O'
