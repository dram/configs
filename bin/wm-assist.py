#!/bin/env python

#
# wm-assist.py
# 
# Author: Xin Wang <dram.wang@gmail.com>
#

import os
import sys
import logging
import optparse
import subprocess
import logging.handlers

import Xlib.X
import Xlib.Xatom
import Xlib.display
import Xlib.protocol.event

PIPE_FILE = "/tmp/wm-assist.pipe"
PID_FILE = "/tmp/wm-assist.pid"
LOG_FILE = "/home/dram/.wm-assist.log"

_NET_WM_STATE_REMOVE = 0
_NET_WM_STATE_ADD = 1
_NET_WM_STATE_TOGGLE = 2

is_tiling = False
orig_geom = None


def get_active_window(dpy, root):
    ids = root.get_full_property(
            dpy.intern_atom("_NET_ACTIVE_WINDOW"), Xlib.Xatom.WINDOW).value
    return dpy.create_resource_object("window", ids[0])

def list_window(dpy, root):
    ids = root.get_full_property(
            dpy.intern_atom("_NET_CLIENT_LIST"), Xlib.Xatom.WINDOW).value
    for win_id in ids:
        win = dpy.create_resource_object("window", win_id)
        print win.get_wm_class()

def move_window(dpy, root, dx, dy):
    win = get_active_window(dpy, root)
    geom = win.get_geometry()
    trans = win.translate_coords(root, geom.x, geom.y)
    x, y = -trans.x, -trans.y

    root.send_event(
            Xlib.protocol.event.ClientMessage(
                window = win,
                client_type = dpy.intern_atom("_NET_MOVERESIZE_WINDOW"),
                data = (32, ([1<<8|1<<9, max(0,x+dx), max(0,y+dy), 0, 0]))),
            Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)
    dpy.sync()

def center_window(dpy, root):
    scr = dpy.screen()
    win = get_active_window(dpy, root)
    geom = win.get_geometry()
    x = max(0, (scr.width_in_pixels - geom.width) / 2)
    y = max(0, (scr.height_in_pixels - geom.height) / 2)

    root.send_event(
            Xlib.protocol.event.ClientMessage(
                window = win,
                client_type = dpy.intern_atom("_NET_MOVERESIZE_WINDOW"),
                data = (32, ([1<<8|1<<9, x, y, 0, 0]))),
            Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)
    dpy.sync()

def tiling(dpy, root):
    def move_resize(win, x, y, w, h):
        # Using StaticGravity to calculate more acculately.
        x, y = max(0, x), max(0, y)
        root.send_event(
                Xlib.protocol.event.ClientMessage(
                    window = win,
                    client_type = dpy.intern_atom("_NET_MOVERESIZE_WINDOW"),
                    data = (32, ([Xlib.X.StaticGravity|1<<8|1<<9|1<<10|1<<11,
                                        x, y, w, h]))),
                Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)

    global is_tiling
    global orig_geom

    wins = {}

    ids = root.get_full_property(
            dpy.intern_atom("_NET_CLIENT_LIST"), Xlib.Xatom.WINDOW).value

    for win_id in ids:
        win = dpy.create_resource_object("window", win_id)
        cls = win.get_wm_class()
        if cls == ('gvim', 'Gvim'):
            wins['editor'] = win
        elif cls == ('sakura', 'Sakura'):
            wins['terminal'] = win
        elif cls == ('midori', 'Midori'):
            wins['browser'] = win

    if not is_tiling:
        scr = dpy.screen()
        win = get_active_window(dpy, root)
        geom = win.get_geometry()
        x = max(0, (scr.width_in_pixels - geom.width) / 2)
        y = max(0, (scr.height_in_pixels - geom.height) / 2)

        orig_geom = {}
        dest = {
                'browser': [0, 0,
                    scr.width_in_pixels, scr.height_in_pixels / 2],
                'terminal': [0, scr.height_in_pixels / 2,
                    scr.width_in_pixels / 2, scr.height_in_pixels / 2],
                'editor': [
                    scr.width_in_pixels / 2, scr.height_in_pixels / 2,
                    scr.width_in_pixels / 2, scr.height_in_pixels / 2]
                }
        
        for prog in ['browser', 'terminal', 'editor']:
            if prog in wins:
                g = wins[prog].get_geometry()
                trans = wins[prog].translate_coords(root, 0, 0)
                orig_geom[prog] = [-trans.x, -trans.y, g.width, g.height]
                x, y, w, h = dest[prog]
                move_resize(wins[prog], x, y, w, h)

        dpy.sync()
    else:
        for prog in ['browser', 'terminal', 'editor']:
            if prog in wins and prog in orig_geom:
                x, y, w, h = orig_geom[prog]
                move_resize(wins[prog], x, y, w, h)

        dpy.sync()
    is_tiling = not is_tiling

def maximize_window(dpy, root):
    win = get_active_window(dpy, root)

    action = _NET_WM_STATE_TOGGLE
    vert = dpy.intern_atom("_NET_WM_STATE_MAXIMIZED_VERT")
    horz = dpy.intern_atom("_NET_WM_STATE_MAXIMIZED_HORZ")

    root.send_event(
            Xlib.protocol.event.ClientMessage(
                window = win,
                client_type = dpy.intern_atom("_NET_WM_STATE"),
                data = (32, ([action, horz, vert, 0, 0]))),
            Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)
    dpy.sync()

def close_window(dpy, root):
    win = get_active_window(dpy, root)

    root.send_event(
            Xlib.protocol.event.ClientMessage(
                window = win,
                client_type = dpy.intern_atom("_NET_CLOSE_WINDOW"),
                data = (32, ([0, 0, 0, 0, 0]))),
            Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)
    dpy.sync()

def resize_window(dpy, root, dw, dh):
    win = get_active_window(dpy, root)
    geom = win.get_geometry()
    w, h = geom.width, geom.height

    root.send_event(
            Xlib.protocol.event.ClientMessage(
                window = win,
                client_type = dpy.intern_atom("_NET_MOVERESIZE_WINDOW"),
                data = (32, ([1<<10|1<<11, 0, 0, max(0,w+dw), max(0,h+dh)]))),
            Xlib.X.SubstructureRedirectMask | Xlib.X.SubstructureNotifyMask)
    dpy.sync()

def jump_or_exec(dpy, root, cls, cmd):
    ids = root.get_full_property(
            dpy.intern_atom("_NET_CLIENT_LIST"), Xlib.Xatom.WINDOW).value

    for win_id in ids:
        win = dpy.create_resource_object("window", win_id)
        if win.get_wm_class() == tuple(cls.split('.')):
            win.configure(stack_mode=Xlib.X.Above)
            win.set_input_focus(Xlib.X.RevertToNone, Xlib.X.CurrentTime)
            break
    else:
        subprocess.Popen(cmd, shell=True, cwd=os.getenv('HOME'))
    dpy.sync()

def main_loop():
    try:
        os.mkfifo(PIPE_FILE)
    except OSError:
        pass

    # Open and hold for a writer end of pipe, so reader end will not enounter
    # EOF.
    hold_pipe = open(PIPE_FILE, "w+")

    dpy = Xlib.display.Display()
    root = dpy.screen().root

    fp = open(PIPE_FILE)

    while True:
        line = fp.readline()

        try:
            cmd, args = line.strip().split(' ', 1)
        except ValueError:
            cmd = line.strip()

        if cmd == 'MOVE':
            dx, dy = args.split()
            move_window(dpy, root, int(dx), int(dy))
        elif cmd == 'RESIZE':
            dw, dh = args.split()
            resize_window(dpy, root, int(dw), int(dh))
        elif cmd == 'JOE':
            cls, cmd = args.split(None, 1)
            jump_or_exec(dpy, root, cls, cmd)
        elif cmd == 'MAXIMIZE':
            maximize_window(dpy, root)
        elif cmd == 'CLOSE':
            close_window(dpy, root)
        elif cmd == 'CENTER':
            center_window(dpy, root)
        elif cmd == 'ALL':
            tiling(dpy, root)

    dpy.close()

def logger():
    return logging.getLogger("WMAssist")

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

def restart_daemon():
    kill_daemon()
    start_daemon()

def main():
    parser = optparse.OptionParser()
    parser.add_option("-d", dest="daemon",
            action="store_true", default=False, help="daemonize process")
    parser.add_option("-D", dest="restart_daemon",
            action="store_true", default=False, help="restart daemon process")
    parser.add_option("-k", dest="kill",
            action="store_true", default=False, help="kill process if exist")

    (options, args) = parser.parse_args()

    logger().setLevel(logging.DEBUG)
    handle = logging.handlers.RotatingFileHandler(
            LOG_FILE, maxBytes=10*1024*1024, backupCount=3)
    handle.setFormatter(logging.Formatter(
        "%(asctime)s %(levelname)s %(message)s", "%m-%d %H:%M:%S"))
    logger().addHandler(handle)

    if options.kill:
        kill_daemon()
        sys.exit(0)

    if options.daemon:
        start_daemon()
    elif options.restart_daemon:
        restart_daemon()

    main_loop()

if __name__ == "__main__":
    main()
    try:
        main()
    except Exception:
        logger().exception("Crash from main()")
