# This shell script is run before Openbox launches.
# Environment variables set here are passed to the Openbox session.

export LANG=en_US.utf8

echo -ne '\033%G'

export PATH=$HOME/bin:/sbin:/usr/sbin:$PATH

export GTK_IM_MODULE=uim
export XMODIFIERS=@im=uim
export QT_IM_MODULE=uim

install -dm700 /dev/shm/firefox-cache
mkdir -p -m777 /dev/shm/firefox-cache/Cache

vim-mpg123.py -d

wm-assist.py -D >>$HOME/.wm-assist.log 2>&1

xchainkeys -d >$HOME/.xchainkeys.log 2>&1 &

#uim-toolbar-gtk-systray &

/usr/sbin/VBoxClient --clipboard &

( source $HOME/.fehbg )

tint2 &

xinput set-int-prop "ImExPS/2 Generic Explorer Mouse" "Evdev Middle Button Emulation" 8 1

# D-bus
if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
       eval `dbus-launch --sh-syntax --exit-with-session`
fi

# Run XDG autostart things.  By default don't run anything desktop-specific
# See xdg-autostart --help more info
DESKTOP_ENV="OPENBOX"
if which /usr/share/openbox/xdg-autostart >/dev/null; then
  /usr/share/openbox/xdg-autostart $DESKTOP_ENV
fi
