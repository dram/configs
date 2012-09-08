#!/bin/bash

export PATH=$HOME/bin:/sbin:/usr/sbin:$PATH

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

case "$-" in *i*)

	export PYTHONSTARTUP=/etc/pythonstart

	case "$TERM" in
	screen)
		trap 'echo -ne "\ek${BASH_COMMAND:0:10}\e\\"' DEBUG
		export PS1='\ekbash\e\\\e[32m\w\e[31m${?/#0}\e[m\n% '
	;;
	*)
		export PS1='\e[32m\w\e[31m${?/#0}\e[m\n% '
	;;
	esac

	alias ls='ls --color'
	alias t='screen -DR'

	;;
esac
