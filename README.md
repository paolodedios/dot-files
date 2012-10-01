dot-files
=========

MacOSX/Linux/Unix configuration files with bootstrap installer functionality. This repository is a heavily modified version of [Mathias
Bynens's dotfile repo](https://github.com/mathiasbynens/dotfiles/).

## overview of files

#### tool configuration
* `.ackrc`          - for ack (better than grep)
* `.inputrc`        - config for bash readline
* `.tmux.conf`      - tmux environment configuration
* `.wgetrc`         - wget configuration

#### shell environement
* `.bashrc`
* `.bash_profile`
* `.bash_exports`
* `.bash_functions`
* `.bash_aliases`
* `.bash_prompt`

#### git
* `.gitconfig`
* `.gitignore_global`

#### mercurial
* `.hgignore_global`

#### editor configurations
* `.vimrc`
* `.vim/`
* `.emacs`
* `.emacs.d/`

#### installer
* `install.sh`


## Installation

### Prerequisites

* Git (1.7+)
* Mercurial (1.6+)
* rsync (3.0+)

### Using Git and the install script

Clone the repository and launch the install script

```bash
git clone https://github.com/paolodedios/dot-files.git && cd dot-files && sh ./install.sh
```

To update, `cd` into your local `dot-files` repository and then:

```bash
sh ./install.sh
```

Alternatively, to update while avoiding the confirmation prompt:

```bash
sh ./install.sh -f
```

### Git-free install

To install these dotfiles without Git:

```bash
cd; curl -#L https://github.com/paolodedios/dot-files/tarball/master | tar -xzv --strip-components 1 --exclude={README.md,install.sh}
```

To update later on, just run that command again.

### Add custom commands without creating a new fork

If `~/.bash_extras` exists, it will be sourced along with the other files. You can use this to add a few custom commands without the need to fork this entire repository, or to add commands you don’t want to commit to a public repository.

My `~/.bash_extras` looks something like this:

```bash
# PATH additions
export PATH="~/bin:$PATH"
```
