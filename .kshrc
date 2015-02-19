#!/bin/ksh

export VISUAL=emacs
export SVN_EDITOR=vi

export LESS_TERMCAP_md='[36m'

export PAGER='less -M +p'

export PKG_PATH=ftp://ftp.openbsd.org/pub/OpenBSD/$(uname -r)/packages/$(uname -p)/

my_prompt () {
	status=$?
	path=${PWD##$HOME*}
	echo -n "[32m${path:-~${PWD#$HOME}}[31m${status#0}[0m\n\$ "
}

PS1='$(my_prompt)'

ulimit -c 0

alias ls='ls -F'
alias e='emacsclient -nw -a ""'
alias t='tmux attach -t - || tmux new -s -'

