#!/bin/bash

alias vim='vim -u NONE'
xmodmap ~/.caps_unswap
setxkbmap us
set -o emacs
export PS1='(Indeed project dir is $INDEED_PROJECT_DIR) [\u@\h \w]\$ '
