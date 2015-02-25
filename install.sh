#!/usr/bin/bash
dotdir=~/dotfiles
export DOTFILES_PROFILE=$1
. $dotdir/common/bashrc
export PATH=$PATH:$dotdir/bin
if [ ! -e ~/.vimrc ]
then
    ln -s $dotdir/common/vimrc ~/.vimrc
fi
if [ ! -e ~/.vim ]
then
    mkdir -p ~/.vim/bundle/vundle
    git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    vim +BundleInstall +qall
fi
if [ ! -e ~/.xmonad ]
then
    ln -s $dotdir/common/xmonad ~/.xmonad
fi
if [ ! -e ~/.pentadactylrc ]
then
    ln -s $dotdir/common/pentadactylrc ~/.pentadactylrc
fi
if [ ! -e ~/.caps_swap ]
then
    ln -s $dotdir/common/caps_swap ~/.caps_swap
fi
if [ ! -e ~/.caps_unswap ]
then
    ln -s $dotdir/common/caps_unswap ~/.caps_unswap
fi
if [ ! -e ~/.Xdefaults ]
then
    ln -s $dotdir/common/Xdefaults ~/.Xdefaults
fi
if [ ! -e ~/.bash_profile ]
then
    ln -s $dotdir/common/bash_profile ~/.bash_profile
fi
