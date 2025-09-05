if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[[ -s "/Users/kshibata101/.gvm/scripts/gvm" ]] && source "/Users/kshibata101/.gvm/scripts/gvm"
