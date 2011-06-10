#!/usr/bin/bash
dotdir=~/dotfiles
echo ". $dotdir/.bashrc" >> ~/.bashrc
ln -s $dotdir/.vimrc ~/.vimrc
ln -s $dotdir/.vim ~/.vim
ln -s $dotdir/.xmonad ~/.xmonad
ln -s $dotdir/.pentadactylrc ~/.pentadactylrc
