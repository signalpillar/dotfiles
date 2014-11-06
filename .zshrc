# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="wedisagree"
ZSH_THEME="sunrise"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew docker git-extras osx pip python vagrant zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# aliases
# some more ls aliases
# t - sort by time
alias ll='ls -altF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias ex='emacs -nw'
alias gitk='gitk --date-order'

source ~/.env
source ~/functions.sh
export proj=$r/proj
export dt=$r/dt
export MAVEN=$dt/apache-maven
export SCALA_HOME=$dt/scala
export PATH=$PATH:$MAVEN/bin:$SCALA_HOME/bin/:~/bin:~/bin/sbt

# editor
export LS_COLORS="ow=01;90:di=01;90:ln=04;90"
export ALTERNATE_EDITOR=""
export EDITOR=~/bin/ec


. `brew --prefix`/etc/profile.d/z.sh
. /usr/local/bin/virtualenvwrapper.sh

alias setup="python setup.py"

# export TERM=xterm-256color
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


export PIP_DOWNLOAD_CACHE=$HOME/Library/Caches/pip-downloads

bindkey -e
bindkey '\e\e[C' forward-word
bindkey '\e\e[D' backward-word
