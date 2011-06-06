#!/usr/bin/env python
# vim: set fileencoding=utf-8

import os
import sys
import time
import select
import logging
import os.path
import optparse
import subprocess
import logging.handlers

PIPE_FILE = "/tmp/vim-mpg123.pipe"
PID_FILE = "/tmp/vim-mpg123.pid"
LOG_FILE = "/home/dram/.vim-mpg123.log"
PLAYLIST_FILE = "/tmp/vim-mpg123-playlist.txt"

def logger():
    return logging.getLogger("VimMPG123")

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
                artist, album = artist_and_album.split(' - ', 1)
            else:
                artist, album = artist_and_album, ''
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

class MPG123Daemon():
    def __init__(self):
        self.p = self.open_mpg123()
        self.playlist = Playlist()

        try:
            os.mkfifo(PIPE_FILE)
        except OSError:
            pass
        
        # Open and hold for a writer end of pipe, so reader end will not
        # enounter EOF.
        self._hold_pipe = open(PIPE_FILE, "w+")

        self.pipe = open(PIPE_FILE, "r", 0)

        self.stoped = False
        self.manually_stop = False

    def open_mpg123(self):
        p = subprocess.Popen(["mpg123", "--remote"],
                stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        p.stdin.write("SILENCE\n")
        return p

    def set_next_track(self):
        track = self.playlist.get_next()
        if track:
            self.stoped = False
            self.p.stdin.write("LOAD %s\n" % track)
            return True
        else:
            self.stoped = True
            return False

    def main_loop(self):
        while True:
            fp = select.select([self.p.stdout, self.pipe], [], [])[0][0]

            if fp == self.p.stdout:
                self.do_state()
            elif fp == self.pipe:
                self.do_command()

    def do_state(self):
        line = self.p.stdout.readline()

        if line == '':
            logger().info('mpg123 process exits abnormally, restart it')
            self.p = self.open_mpg123()
            return

        logger().info('state %s', line.strip())

        if line.strip() == '':
            time.sleep(1)

        if line.startswith('@P 0'):
            # current track finished
            if not self.manually_stop:
                self.set_next_track()
            self.manually_stop = False

    def cmd(self, op):
        self.p.stdin.write(op.strip() + '\n')

    def do_command(self):
        line = self.pipe.readline()

        logger().info('command %s', line.strip())

        try:
            cmd, arg = line.strip().split(' ', 1)
        except ValueError:
            cmd = line.strip()

        if cmd == 'CLEAR':
            self.playlist.clear()
            self.cmd('STOP')
        elif cmd == 'STOP':
            self.manually_stop = True
            self.cmd('STOP')
        elif cmd == 'PAUSE':
            self.cmd('PAUSE')
        elif cmd == 'ADD':
            self.playlist.add(arg)
            if self.stoped:
                self.set_next_track()
        elif cmd == 'PLAY':
            self.cmd('PAUSE')
        elif cmd == 'NEXT':
            self.set_next_track()
        elif cmd == 'SEEK':
            if self.playlist.seek(int(arg)):
                self.set_next_track()
        elif cmd == 'PREVIOUS':
            if self.playlist.seek_previous():
                self.set_next_track()

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

if __name__ == '__main__':
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

    if options.daemon:
        handle = logging.handlers.RotatingFileHandler(
                LOG_FILE, maxBytes=10*1024*1024, backupCount=3)
    else:
        handle = logging.StreamHandler()

    handle.setFormatter(logging.Formatter(
        "%(asctime)s %(levelname)s %(message)s", "%m-%d %H:%M:%S"))
    logger().addHandler(handle)

    if options.daemon:
        try:
            MPG123Daemon().main_loop()
        except Exception:
            logger().exception("Crash from main()")
    else:
        MPG123Daemon().main_loop()
