bindkey -e
export LANG=ja_JP.UTF-8
export PATH=/usr/local/bin:$PATH

# alias
# 使い方
# % ls -al L       # ls -al | less と同じ
# % ps -ef G zsh   # ps -ef | grep zsh と同じ
alias -g L='| less'
alias -g G='| grep'
alias -g H='| head'
alias -g T='| tail'
alias ls='ls -FG'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias ks='ls'
alias .="ls"
alias ..="cd ../"
alias ...="cd ../../"
alias ....="cd ../../../"
# mino monta
alias mino='node'
alias so='source'
alias soz='source ~/.zshrc'
alias g11='g++ --std=c++11'
alias g11d='g++ --std=c++11 -g -O0'
alias be='bundle exec '

###
function chpwd() { ls }

set -o ignoreeof

##プロンプト
PROMPT="%F{yellow}[%~]%f
$ "
# setopt transient_rprompt

##ヒストリ強化
HISFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history

##補完強化
autoload -U compinit
compinit

# 補完に関するオプション
# http://voidy21.hatenablog.jp/entry/20090902/1251918174
setopt always_last_prompt    # カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt complete_in_word      # 語の途中でもカーソル位置で補完
setopt auto_param_slash      # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt mark_dirs             # ファイル名の展開でディレクトリにマッチした場合 末尾に / を付加
setopt list_types            # 補完候補一覧でファイルの種別を識別マーク表示 (訳注:ls -F の記号)
setopt auto_menu             # 補完キー連打で順に補完候補を自動で補完
setopt auto_param_keys       # カッコの対応などを自動的に補完
setopt interactive_comments  # コマンドラインでも # 以降をコメントと見なす
setopt magic_equal_subst     # コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt auto_list

zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' verbose yes # 補完関数の表示を強化する
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*:messages' format '%F{YELLOW}%d'$DEFAULT
zstyle ':completion:*:warnings' format '%F{RED}No matches for:''%F{YELLOW} %d'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:descriptions' format '%F{yellow}Completing %B%d%b%f'$DEFAULT
zstyle ':completion:*' group-name '' # マッチ種別を別々に表示
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters # 変数の添字を補完する
zstyle ':completion:*:manuals' separate-sections true # manの補完をセクション番号別に表示させる


##コアダンプサイズ制限
limit coredumpsize 102400

##配色
# setopt prompt_subst
export LSCOLORS=gxfxcxdxbxegedabagacad

##スペルチェック
# setopt correct

##cd 強化
setopt auto_cd
setopt autopushd
setopt pushd_ignore_dups

## zsh-completion
fpath=(path/to/zsh-completions/src $fpath)

# 範囲指定できるようにする
# 例 : mkdir {1-3} で フォルダ1, 2, 3を作れる
setopt brace_ccl


###
### http://mollifier.hatenablog.com/entry/20090413/1239551651
###

### コマンドスタック
### http://qiita.com/items/1f2c7793944b1f6cc346
show_buffer_stack() {
  POSTDISPLAY="
stack: $LBUFFER"
  zle push-line-or-edit
}
zle -N show_buffer_stack
setopt noflowcontrol
bindkey '^Q' show_buffer_stack

### 単語削除
WORDCHARS=${WORDCHARS:s,/,,}

### 空白時タブでバッファにls表示
function expand-or-complete-or-list-files() {
    if [[ $#BUFFER == 0 ]]; then
        BUFFER="cd "
	CURSOR=3
        zle list-choices
    else
        zle expand-or-complete
    fi
}
zle -N expand-or-complete-or-list-files
# bind to tab
bindkey '^I' expand-or-complete-or-list-files

# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

# gitなどバージョン管理
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
        psvar=()
            LANG=en_US.UTF-8 vcs_info
                [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
eval "$(pyenv init -)"

# goenv
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
eval "$(goenv init -)"

## create emacs env file
#perl -wle \
#    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
#        PATH > ~/.emacs.d/shellenv.el

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

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
