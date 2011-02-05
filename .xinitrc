#!/bin/sh

xrdb --merge $HOME/.Xresources

install -dm700 /dev/shm/firefox-cache

xchainkeys >/dev/null 2>&1 &

vim-gst-srv.py -d

wm-assist.py -D

sleep 3 && eval $(cat $HOME/.fehbg) && tint2 &

openbox-session
