#!/bin/ksh

export LANG=en_US.UTF-8

export VISUAL=emacs

export GIT_EDITOR=vi

export SVN_EDITOR=vi

export LESS_TERMCAP_md='[36m'

export PAGER='less -M +p'

export PKG_PATH=ftp://ftp.netbsd.org/pub/pkgsrc/packages/NetBSD/$(uname -m)/$(uname -r)/All/

# Support 256 colors for putty & emacs & screen.
# http://stackoverflow.com/a/22088744
export TERMCAP=

my_prompt () {
	status=$?
	path=${PWD##$HOME*}
	echo -n "[32m${path:-~${PWD#$HOME}}[31m${status#0}[0m\n\$ "
}

case "$-" in *i*)
        PS1='$(my_prompt)'

        ulimit -c 0

        alias ls='ls -F'
        alias e='emacsclient -nw -a ""'
        alias t='screen -DR'

        ;;
esac
