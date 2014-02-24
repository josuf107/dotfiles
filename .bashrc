#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PS1='[\u@\h \W]\$ '
export PATH=$PATH:~/bin:~/.cabal/bin:~/cabal-dev/bin
export EDITOR=vim
set -o vi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

shopt -s checkwinsize

# Common aliases
alias ls='ls --color=auto'
alias bv='vim ~/.bashrc && . ~/.bashrc'
alias bs='. ~/.bashrc'
alias tvim='vim -u .vimrc'
alias hs='. .hsenv/bin/activate'
