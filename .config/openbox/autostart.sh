# This shell script is run before Openbox launches.
# Environment variables set here are passed to the Openbox session.

eval $(cat $HOME/.fehbg)

tint2 &

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
