#!/bin/sh

 DOT_FILES=(.tmux.conf .zshrc .zshenv)

for file in ${DOT_FILES[@]}
do
    if [ ! -f "$HOME/$file" ]; then
        ln -s $HOME/dotfiles/$file $HOME/$file
    fi
done
ln -s $HOME/dotfiles/.tmux $HOME/.tmux

#setup for nvim
if [ ! -d "$HOME/.config" ]; then
    mkdir $HOME/.config
fi

# install vim config
ln -s $HOME/dotfiles/nvim $HOME/.config/nvim
ln -s $HOME/dotfiles/nvim $HOME/.vim
ln -s $HOME/dotfiles/nvim/init.vim $HOME/.vimrc
ln -s $HOME/dotfiles/alacritty $HOME/.config/alacritty
