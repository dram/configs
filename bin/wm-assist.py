#!/usr/bin/env python2

#
# wm-assist.py
# 
# Author: Xin Wang <dram.wang@gmail.com>
#

import os
import subprocess

import Xlib.X
import Xlib.Xatom
import Xlib.display
import Xlib.protocol.event

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
        elif cls == ('evilvte', 'Evilvte'):
            wins['terminal'] = win
        elif cls == ("Navigator", "Firefox"):
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

class Assist(object):
    def __init__(self):
        self.connect()

    def connect(self):
        self.dpy = Xlib.display.Display()
        self.root = self.dpy.screen().root

    def move_left(self):
        move_window(self.dpy, self.root, -20, 0)

    def move_right(self):
        move_window(self.dpy, self.root, 20, 0)

    def move_up(self):
        move_window(self.dpy, self.root, 0, -20)

    def move_down(self):
        move_window(self.dpy, self.root, 0, 20)

    def joe(self, cls, cmd):
        jump_or_exec(self.dpy, self.root, cls, cmd)

    def maximize(self):
        maximize_window(self.dpy, self.root)

    def close(self):
        close_window(self.dpy, self.root)

    def center(self):
        center_window(self.dpy, self.root)

    def all(self):
        tiling(self.dpy, self.root)

    def enlarge_width(self):
        resize_window(self.dpy, self.root, 20, 0)

    def reduce_width(self):
        resize_window(self.dpy, self.root, -20, 0)

    def enlarge_height(self):
        resize_window(self.dpy, self.root, 0, 20)

    def reduce_height(self):
        resize_window(self.dpy, self.root, 0, -20)


if __name__ == "__main__":
    if os.getenv('PYTHONINSPECT'):
        a = Assist()

        del os.environ['TMUX']
    else:
        try:
            subprocess.check_call(['tmux',
                                   'new-session','-d', '-s', 'wm-assist',
                                   'PYTHONINSPECT=y python %s' % __file__])
        except subprocess.CalledProcessError:
            subprocess.call(['tmux', 'send-keys', '-t', 'wm-assist',
                             'a.connect()', 'C-m'])
