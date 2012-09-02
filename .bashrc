#!/bin/bash

export PATH=/sbin:/usr/sbin:$PATH

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd'

case "$-" in *i*)

	export PYTHONSTARTUP=/etc/pythonstart

	export PS1='\e[32m\w\e[31m${?/#0}\e[m\n% '

	alias ls='ls --color'
	alias t='screen -DR'

	;;
esac
