#!/usr/bin/bash
dotdir=~/dotfiles
. $dotdir/.bashrc
export PATH=$PATH:$dotdir/bin
if [ ! -e ~/.vimrc ]
then
    ln -s $dotdir/.vimrc ~/.vimrc
fi
if [ ! -e ~/.vim ]
then
    ln -s $dotdir/.vim ~/.vim
fi
if [ ! -e ~/.xmonad ]
then
    ln -s $dotdir/.xmonad ~/.xmonad
fi
if [ ! -e ~/.pentadactylrc ]
then
    ln -s $dotdir/.pentadactylrc ~/.pentadactylrc
fi
if [ ! -e ~/.caps_swap ]
then
    ln -s $dotdir/.caps_swap ~/.caps_swap
fi
if [ ! -e ~/.caps_unswap ]
then
    ln -s $dotdir/.caps_unswap ~/.caps_unswap
fi
if [ ! -e ~/.Xdefaults ]
then
    ln -s $dotdir/.Xdefaults ~/.Xdefaults
fi
if [ ! -e ~/.bash_profile ]
then
    ln -s $dotdir/.bash_profile ~/.bash_profile
fi
