# .zshrc

HISTFILE=~/.zshis
HISTSIZE=100
SAVEHIST=100
PATH+=:$HOME/bin:/usr/sbin:/sbin

export EDITOR=vim

unsetopt beep

# Copy from Aaron Toponce's blog.
# URL: http://pthree.org/2009/03/28/add-vim-editing-mode-to-your-zsh-prompt/
function zle-keymap-select {
    VIMODE="${${KEYMAP/vicmd/C}/(main|viins)/}"
    zle reset-prompt
}

zle -N zle-keymap-select

setopt prompt_subst
PS1='${VIMODE}'$'%{\e[32m%}%~%{\e[31m%}%(0?..%?)%{\e[0m%}%# '

# Change to softcursor.
#TERM=linux
#echo -e '\033[?18;127;8c'

bindkey -v

export LESS_TERMCAP_md=$'\E[36m'

alias ls='ls -F --color'
alias vi=vim
alias grep='grep -nH --color=auto'
alias codecolor='pygmentize -f html'

