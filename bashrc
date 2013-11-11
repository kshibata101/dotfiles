export JAVA_HOME=/Library/Java/Home
#export DYLD_LIBRARY_PATH=/usr/local/mysql/lib
export PS1="[\w]: "
export M2_HOME=/usr/local/apache-maven-3.0.4
export M2=$M2_HOME/bin
export PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}\007"'
export PHPBREW_SET_PROMPT=1
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:$JAVA_HOME/bin
export PATH=$PATH:/usr/local/mysql/bin
export PATH=$PATH:/Users/kazukishibata/Documents/lib/scala-2.9.1.final/bin
export PATH=$PATH:/Applications/XAMPP/xamppfiles
export PATH=$PATH:/usr/local/play-2.0.4
export PATH=$PATH:/usr/local/WordNet-3.0/bin
export PATH=$PATH:~/bin
export PATH=$PATH:$M2
export PATH=$PATH:~/pear/bin
export PATH=$PATH:/usr/local/share/npm/lib/node_modules
export PATH=$PATH:~/Documents/lib/adt-bundle-mac-x86_64-20130219/sdk/platform-tools:~/Documents/lib/adt-bundle-mac-x86_64-20130219/sdk/tools

alias fn='find . -name'
alias dd="cdls"
alias rm='rm -i'
alias .='ls .'
alias ..='dd ../'
alias ...='dd ../../'
alias ....='dd ../../../'
alias tm='tmux'
alias ta='tmux a'
alias tl='tmux ls'
alias pg='ps aux | grep'

function cdls() {
    \cd $*;
    ls;
}

case "${OSTYPE}" in
darwin*)
  alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
  alias xampp="xampp"
  export LSCOLORS=gxfxcxdxbxegedabagacad
  alias ls='ls -FG'
  alias l='ls'
  alias la='ls -a'
  alias ll='ls -l'
  alias lla='ls -la'
  ;;
linux*)
  alias ls='ls --color'
  alias ll='ls -l --color'
  alias la='ls -la --color'
  alias lla='ls -lla --color'
  ;;
esac

[[ -s /Users/kshibata101/.nvm/nvm.sh ]] && source /Users/kshibata101/.nvm/nvm.sh # This loads NVM
nvm use v0.10.4

# Load RVM into a shell session *as a function
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

### Added by the Heroku Toolbelt
export PATH="$PATH:/usr/local/heroku/bin"

[[ -s "$HOME/.phpbrew/bashrc" ]] && source "$HOME/.phpbrew/bashrc"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
