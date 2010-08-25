#!/bin/ksh

export VISUAL=emacs

export LESS_TERMCAP_md='[36m'

export PAGER='less -M +p'

export PATH=$PATH:/usr/local/Gambit-C/bin

my_prompt () {
	status=$?
	path=${PWD##$HOME*}
	echo -n "[32m${path:-~${PWD#$HOME}}[31m${status#0}[0m\n\$ "
}

PS1='$(my_prompt)'

alias ls='ls -F'

