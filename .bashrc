#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
export PS1='[\u@\h \W]\$ '
export PATH=$PATH:/home/joseph07/bin
export EDITOR=/usr/bin/vim
