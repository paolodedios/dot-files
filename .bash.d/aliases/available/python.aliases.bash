#
# Python/Virtualenv related aliases
#
########################################################################################

cite 'about-alias'
about-alias 'python/virtualenv abbreviations'

########################################################################################
# Python virtualenvwrapper aliases
########################################################################################

# Create a new virtual environment
alias pymkenv="mkvirtualenv"

# Switch to a specific virtual environment
alias pystartenv="workon"

# Stop using the current virtual environment
alias pystopenv="deactivate && export PYTHON_VIRTUALENV_TOPLEVEL="

# List virtual environments
alias pylsenv="lsvirtualenv -b | sort"

# Remove virtual environment
alias pyrmenv="rmvirtualenv"

# Copy virtual environment
alias pycpenv="cpvirtualenv"

# Change dir to top level of current virtual environment
alias pycdenv="cdvirtualenv"

# List site-packages for current virtual environment
alias pylsenvpkgs="lssitepackages"

# Update site-packages via pip
alias pyupdatepkgs='pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U'

# Generate python package list
alias pyfreezepkgs="pip freeze --local | grep -v '^\-e' > requirements.txt"

# Install python packages from list
alias pyinstallpkgs="pip install -r"

# Upgrade specific package
alias pyupdate='pip install -U'
