# .zshrc

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin

export EDITOR=vim

unsetopt beep

# From http://zshwiki.org/home/examples/zlewidgets
function zle-line-init zle-keymap-select {
	RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
	RPS2=$RPS1
	zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

setopt prompt_subst
PS1='${VIMODE}'$'%{\e[32m%}%~%{\e[31m%}%(0?..%?)%{\e[0m%}%# '

# Copy from http://aperiodic.net/phil/prompt/
# Dynamically change screen's title to recently command name.
setopt extended_glob
preexec () {
	if [[ "$TERM" == "screen" ]]; then
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

