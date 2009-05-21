# .zshrc

LANG=en_US

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin

export EDITOR=vim

unsetopt beep

# From http://zshwiki.org/home/examples/zlewidgets
VIMODE=0
function zle-line-init zle-keymap-select {
	VIMODE="${${KEYMAP/vicmd/32}/(main|viins)/0}"
	zle reset-prompt
}

if [ ${ZSH_VERSION%.?} != "4.2" ]
then
	zle -N zle-line-init
	zle -N zle-keymap-select
fi

setopt prompt_subst
PS1=$'%{\e[32m%}%~%{\e[31m%}%(0?..%?)%{\e[${VIMODE}m%}%#%{\e[0m%} '

# Copy from http://aperiodic.net/phil/prompt/
# Dynamically change screen's title to recently command name.
setopt extended_glob
preexec () {
	if [[ ${TERM#screen} != ${TERM} ]]; then
		local CMD=${1[(wr)^(*=*|sudo|-*)]}
		echo -ne "\ek$CMD\e\\"
	fi
}

# Change to softcursor.
#TERM=linux
#echo -e '\033[?18;127;8c'

bindkey -v

export LESS_TERMCAP_md=$'\E[36m'

alias ls='ls -F --color'
alias vi=vim
alias grep='grep -nH --color=auto'
alias codecolor='pygmentize -f html'

