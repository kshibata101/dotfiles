#!/bin/sh
while getopts a OPT
do
    case $OPT in
	"a" ) ALL="TRUE" ;;
    esac
done

parent_path=$(cd $(dirname $(dirname $0)) && pwd)

ln -s ${parent_path}/.bash_profile ~/.bash_profile
ln -s ${parent_path}/.bashrc ~/.bashrc
ln -s ${parent_path}/.gitconfig ~/.gitconfig
ln -s ${parent_path}/.gitignore ~/.gitignore
ln -s ${parent_path}/.tmux.conf ~/.tmux.conf
ln -s ${parent_path}/.vimrc ~/.vimrc
ln -s ${parent_path}/.zshrc ~/.zshrc
ln -s ${parent_path}/.pryrc ~/.pryrc

if [ "ALL" = "TRUE" ]; then
    ln -s ${parent_path}/.emacs.d/ ~/.emacs.d
    ln -s ${parent_path}/.vimperatorrc ~/.vimperatorrc
fi
