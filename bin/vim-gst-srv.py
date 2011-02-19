#!/usr/bin/env python
# vim: set fileencoding=utf-8

import os
import sys
import glib
import thread
import select
import gobject
import logging
import os.path
import optparse
import logging.handlers

import pygst
pygst.require("0.10")
import gst

PIPE_FILE = "/tmp/vim-gst.pipe"
PID_FILE = "/tmp/vim-gst-srv.pid"
LOG_FILE = "/home/dram/.vim-gst-srv.log"
PLAYLIST_FILE = "/tmp/vim-gst-playlist.txt"

def logger():
    return logging.getLogger("VimGST")

class Playlist():
    def __init__(self):
        self.lst = []
        self.nxt = 0

    def _update_file(self):
        fp = open(PLAYLIST_FILE, "w")
        cur = self.nxt - 1
        for i in range(len(self.lst)):
            if i == cur:
                mark = "-"
            else:
                mark = " "
            track = os.path.splitext(os.path.basename(self.lst[i]))[0]
            artist_and_album = os.path.basename(os.path.dirname(self.lst[i]))
            if ' - ' in artist_and_album:
                artist, album = artist_and_album.split(' - ')
            else:
                artist, album = '', artist_and_album
            fp.write("%s%s | %s\n" % (mark, artist, track))
        fp.close()

    def add(self, path):
        path = path.strip()

        if os.path.isfile(path):
            self.lst.append(path)
        elif os.path.isdir(path):
            for entry in os.listdir(path):
                p = os.path.join(path, entry)
                if os.path.isfile(p):
                    self.lst.append(p)
        self._update_file()

    def clear(self):
        self.lst = []
        self.nxt = 0
        self._update_file()

    def seek(self, idx):
        if idx >= 0 and idx < len(self.lst):
            self.nxt = idx
            self._update_file()
            return True
        else:
            return False

    def seek_previous(self):
        if self.nxt - 2 >= 0:
            self.nxt -= 2
            self._update_file()
            return True
        else:
            return False

    def is_empty(self):
        return len(self.lst) == 0

    def get_next(self):
        if self.nxt >= len(self.lst):
            return None
        else:
            track = self.lst[self.nxt]
            self.nxt += 1
            self._update_file()
            return track

class GSTPlayDaemon():
    def __init__(self):
        self.player = gst.element_factory_make("playbin2", "player")
        fakesink = gst.element_factory_make("fakesink", "fakesink")
        self.player.set_property("video-sink", fakesink)

        self.player.connect("about-to-finish", self.on_about_to_finish)

        bus = self.player.get_bus()
        bus.add_signal_watch()
        bus.connect("message", self.on_message)

        self.playlist = Playlist()

    def on_message(self, bus, message):
        global g_finished

        t = message.type

        if t == gst.MESSAGE_EOS:
            self.player.set_state(gst.STATE_NULL)
        elif t == gst.MESSAGE_ERROR:
            err, debug = message.parse_error()
            print "Error: %s" % err, debug
            if self.set_next_track():
                self.player.set_state(gst.STATE_PLAYING)
        else:
            pass
            #print message

    def set_next_track(self):
        track = self.playlist.get_next()
        if track:
            self.player.set_property("uri", "file://" + track)
            return True
        else:
            return False

    def on_about_to_finish(self, user_data):
        self.set_next_track()

    def cmd_loop(self):
        try:
            os.mkfifo(PIPE_FILE)
        except OSError:
            pass

        # Open and hold for a writer end of pipe, so reader end will not
        # enounter EOF.
        hold_pipe = open(PIPE_FILE, "w+")

        fp = open(PIPE_FILE)

        while True:
            line = fp.readline()

            logger().info('get %s', line)

            try:
                cmd, arg = line.strip().split(' ', 1)
            except ValueError:
                cmd = line.strip()

            _, state, _ = self.player.get_state()
            if cmd == 'CLEAR':
                self.playlist.clear()
                self.player.set_state(gst.STATE_NULL)
            elif cmd == 'STOP':
                self.player.set_state(gst.STATE_READY)
            elif cmd == 'PAUSE':
                if state == gst.STATE_PLAYING:
                    self.player.set_state(gst.STATE_PAUSED)
                elif state == gst.STATE_PAUSED:
                    self.player.set_state(gst.STATE_PLAYING)
            elif cmd == 'ADD':
                self.playlist.add(arg)

                if state != gst.STATE_PLAYING and not self.playlist.is_empty():
                    self.player.set_state(gst.STATE_NULL)
                    if self.set_next_track():
                        self.player.set_state(gst.STATE_PLAYING)
            elif cmd == 'PLAY':
                if state != gst.STATE_PLAYING:
                    self.player.set_state(gst.STATE_PLAYING)
            elif cmd == 'NEXT':
                self.player.set_state(gst.STATE_NULL)
                if self.set_next_track():
                    self.player.set_state(gst.STATE_PLAYING)
            elif cmd == 'SEEK':
                self.player.set_state(gst.STATE_NULL)
                if self.playlist.seek(int(arg)) and self.set_next_track():
                    self.player.set_state(gst.STATE_PLAYING)
            elif cmd == 'PREVIOUS':
                self.player.set_state(gst.STATE_NULL)
                if self.playlist.seek_previous() and self.set_next_track():
                    self.player.set_state(gst.STATE_PLAYING)

def daemon():
    import os
    import resource

    pid = os.fork()
    if pid != 0:
        os._exit(0) # parent exits.

    os.setsid()

    pid = os.fork()
    if pid != 0:
        os._exit(0) # origin child exits.

    os.umask(0)
    os.chdir("/")

    maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
    if (maxfd == resource.RLIM_INFINITY):
        maxfd = 1024
    
    for fd in range(0, maxfd):
        try:
            os.close(fd)
        except OSError:
            pass

    os.open(os.devnull, os.O_RDWR)
    os.dup2(0, 1)
    os.dup2(0, 2)

def start_daemon():
    import os
    import sys
    import atexit
    import os.path
    if os.path.exists(PID_FILE):
        print >>sys.stderr, "[start] process already exists"
        sys.exit(1)

    def delete_pid_file():
        os.remove(PID_FILE)

    daemon()

    fp = open(PID_FILE, "w")
    fp.write(str(os.getpid()) + "\n")
    fp.close()

    atexit.register(delete_pid_file)

    print >>sys.stderr, "[start] start (pid=%d)" % os.getpid()

def kill_daemon():
    import sys
    import time
    import signal

    try:
        pid = int(open(PID_FILE).read())
    except:
        return

    try:
        while True:
            os.kill(pid, signal.SIGTERM)
            time.sleep(1)
    except OSError:
        print >>sys.stderr, "Kill previous process (pid=%d)." % pid
        os.remove(PID_FILE)

def restart():
    kill_daemon()
    start_daemon()

def main():
    parser = optparse.OptionParser()
    parser.add_option("-d", dest="daemon",
            action="store_true", default=False, help="daemonize process")
    parser.add_option("-k", dest="kill",
            action="store_true", default=False, help="kill process if exist")

    (options, args) = parser.parse_args()

    if options.kill:
        kill_daemon()
        sys.exit(0)

    if options.daemon:
        start_daemon()

    logger().setLevel(logging.DEBUG)
    handle = logging.handlers.RotatingFileHandler(
            LOG_FILE, maxBytes=10*1024*1024, backupCount=3)
    handle.setFormatter(logging.Formatter(
        "%(asctime)s %(levelname)s %(message)s", "%m-%d %H:%M:%S"))
    logger().addHandler(handle)

    play = GSTPlayDaemon()

    thread.start_new_thread(play.cmd_loop, ())

    gobject.threads_init()

    glib.MainLoop().run()

if __name__ == '__main__':
    try:
        main()
    except Exception:
        logger().exception("Crash from main()")
