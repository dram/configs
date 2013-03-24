#!/bin/bash

export PATH=$HOME/bin:/sbin:/usr/sbin:$PATH

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

case "$-" in *i*)

	case "$TERM" in
	screen*)
		trap 'echo -ne "\ek${BASH_COMMAND:0:10}\e\\"' DEBUG
		export PS1='\ekbash\e\\\e[32m\w\e[31m${?/#0}\e[m\n% '
	;;
	*)
		export PS1='\e[32m\w\e[31m${?/#0}\e[m\n% '
	;;
	esac

	export EDITOR=vim
	export LESSCHARSET=utf-8

	export HISTSIZE=3000
	bind '"\e[A": history-search-backward'
	bind '"\e[B": history-search-forward'

	alias ls='ls --color'
	alias t='tmux attach -t - || tmux new -s -'

	;;
esac
