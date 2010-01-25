# .zshrc

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin
MANPATH+=:/usr/local/share/man

export EDITOR=vim

unsetopt beep

bindkey -e

setopt prompt_subst
PS1=$'%{\e[32m%}%~@%{\e[0m%}%m%{\e[31m%}%(0?..%?)%{\e[0m%}%# '

export LESS_TERMCAP_md=$'\E[36m'

alias ls='ls -F --color'
alias vi=vim
alias grep='grep -nH --color=auto'

