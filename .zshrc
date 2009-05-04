# .zshrc

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin
unsetopt beep

PS1=$'%{\e[32m%}%~%{\e[31m%}%(0?..%?)%{\e[0m%}%# '

# Change to softcursor.
#TERM=linux
#echo -e '\033[?18;127;8c'

export LESS_TERMCAP_md=$'\E[36m'

alias ls='ls -F'
alias grep='grep -nH --color=auto'
alias codecolor='pygmentize -f html'

