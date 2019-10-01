#!/bin/sh

 DOT_FILES=(.tmux.conf .tmux .zshrc .zshenv)
 DOT_CONFIG=(nvim alacritty)

for file in ${DOT_FILES[@]}
do
    if [ ! -e "$HOME/$file" ]; then
        ln -s $HOME/dotfiles/$file $HOME/$file
    fi
done

#setup for nvim
if [ ! -d "$HOME/.config" ]; then
    mkdir $HOME/.config
fi

for file in ${DOT_FILES[@]}
do
    if [ ! -e "$HOME/$file" ]; then
        ln -s $HOME/dotfiles/$file $HOME/.config/$file
    fi
done

# install vim config
ln -s $HOME/dotfiles/nvim $HOME/.vim
ln -s $HOME/dotfiles/nvim/init.vim $HOME/.vimrc
