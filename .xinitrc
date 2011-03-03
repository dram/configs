#!/bin/sh

GTK_IM_MODULE=uim ; export GTK_IM_MODULE
QT_IM_MODULE=uim ; export QT_IM_MODULE
XMODIFIERS=@im=uim ; export XMODIFIERS

xrdb --merge $HOME/.Xresources

install -dm700 /dev/shm/firefox-cache

xchainkeys >/dev/null 2>&1 &

vim-gst-srv.py -d

wm-assist.py -D

sleep 5 && uim-toolbar-gtk-systray &


VBoxClient-all &

ck-launch-session openbox-session
