export proj=$r/proj
export dt=$r/dt
export MAVEN=$dt/apache-maven
export SCALA_HOME=$dt/scala
export PATH=~/.pyenv/shims:/usr/local/sbin:/usr/local/bin:$PATH:$MAVEN/bin:$SCALA_HOME/bin/:~/bin:~/bin/sbt

# to enable gnu tools by default
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# Extend PATH to find `go` binaries
export GOPATH=~/go/
export PATH=$PATH:/usr/local/opt/go/libexec/bin:$GOPATH/bin

export PIP_DOWNLOAD_CACHE=$HOME/Library/Caches/pip-downloads

export PROJECT_HOME=~/proj/py/
export PATH="$HOME/.pyenv/bin:$PATH"
