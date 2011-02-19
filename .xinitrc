#!/bin/sh

xrdb --merge $HOME/.Xresources

install -dm700 /dev/shm/firefox-cache

xchainkeys >/dev/null 2>&1 &

vim-gst-srv.py -d

wm-assist.py -D

openbox-session
