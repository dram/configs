#!/bin/sh

xrdb --merge $HOME/.Xresources

xrandr --dpi 98

xmodmap -e "keycode 135 = Super_R"

export QT_IM_MODULE=xim
export GTK_IM_MODULE=xim
export XMODIFIERS="@im=fcitx"
export XIM=fcitx
export XIM_PROGRAM=fcitx

fcitx -d

nohup $HOME/bin/jump-or-exec.py >/dev/null 2>&1 &

openbox-session
