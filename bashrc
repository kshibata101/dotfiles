export PS1="[\w]: "
export PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}\007"'
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:~/bin

alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias fn='find . -name'
alias rm='rm -i'
alias ..='cdls ../'
alias ...='cdls ../../'
alias ....='cdls ../../../'
alias tmux='tmux -u'
alias tm='tmux'
alias ta='tmux a'
alias tl='tmux ls'
alias sc="screen"
alias sr="screen -r"
alias pg='ps aux | grep'
alias g11='g++ --std=c++11'
alias sob='source ~/.bashrc'

function cdls() {
    \cd $*;
    ls;
}

case "${OSTYPE}" in
darwin*)
  export LSCOLORS=gxfxcxdxbxegedabagacad
  alias ls='ls -FG'
  ;;
linux*)
  alias ls='ls --color'
  ;;
esac

set -o ignoreeof

if [ -d ${HOME}/.env/enabled ] ; then
    for file in `\find ${HOME}/.env/enabled -maxdepth 1 -type f -o -type l`; do
        source ${file}
    done
fi
if [ -d ${HOME}/.alias/enabled ] ; then
    for file in `\find ${HOME}/.alias/enabled -maxdepth 1 -type f -o -type l`; do
        source ${file}
    done
fi

if [ -f ~/.bashrc.local ]; then
    source ~/.bashrc.local
fi
