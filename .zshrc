# .zshrc

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin

export EDITOR=vim

unsetopt beep

setopt prompt_subst
PS1=$'%{\e[32m%}%~@%{\e[0m%}%m%{\e[31m%}%(0?..%?)%{\e[0m%}%# '

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

export LESS_TERMCAP_md=$'\E[36m'

alias ls='ls -F --color'
alias vi=vim
alias grep='grep -nH --color=auto'
alias codecolor='pygmentize -f html'

