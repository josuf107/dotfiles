#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
export PS1='[\u@\h \W]\$ '
export PATH=$PATH:~/bin:~/.cabal/bin:~/cabal-dev/bin
export EDITOR=vim
set -o vi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

shopt -s checkwinsize
