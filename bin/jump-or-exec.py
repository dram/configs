#!/usr/bin/python

######################################################################
## file:   jump-or-exec.py
## author: pluskid <pluskid.zju@gmail.com>
## date:   2007-01-25
######################################################################

import re
import subprocess
from Xlib import X, display, Xatom, XK, Xutil, protocol

class JumpOrExec:
    modifiers =  {
        'Control':X.ControlMask,
        'Shift':X.ShiftMask,
        'Mod1':X.Mod1Mask,
        'Alt':X.Mod1Mask,
        'Mod2':X.Mod2Mask,
        'Mod3':X.Mod3Mask,
        'Mod4':X.Mod4Mask,
        'Mod5':X.Mod5Mask
        }
    def __init__(self):
        self.display = display.Display()
        self.root = self.display.screen().root
        (self.capslock_mask, self.numlock_mask, self.scrolllock_mask) = \
            self.get_lock_masks()
        self.jobs = {}
    def get_lock_masks(self):
        """Get the value of capslock_mask, numlock_mask and scrolllock_mask."""
        mask_table = [X.ShiftMask, X.LockMask, X.ControlMask,
                      X.Mod1Mask, X.Mod2Mask, X.Mod3Mask,
                      X.Mod4Mask, X.Mod5Mask]
        nlock = self.display.keysym_to_keycode(XK.XK_Num_Lock)
        slock = self.display.keysym_to_keycode(XK.XK_Scroll_Lock)
        modmap = self.display.get_modifier_mapping()
        slock_mask = 0
        nlock_mask = 0
        for i in range(len(modmap)):
            for key in modmap[i]:
                if key != 0 and key == nlock:
                    nlock_mask = mask_table[i]
                elif key != 0 and key == slock:
                    slock_mask = mask_table[i]
        return (X.LockMask, nlock_mask, slock_mask)
    def install_job(self, key_string, job):
        """
        Install the job with key_string.

        key_string's format is just like that used by `xbindkeys'. But
        the syntax is more restrictly here. Keys should be seperated
        EXACTLY by ' + ', thus 'Control + +' is valid while 'Control++'
        is invalid.

        job is an instance of the Job class.

        If the key is detected, first we will ask the job object to
        find an existing window (See the document for the class Job
        for more information), if found, we will raise it, otherwise
        we will tell the job to start a new process.
        """
        (key_code, modifier) = self.decode_key(key_string)
        self.jobs[(key_code, modifier)] = job
        self.grab_key(key_code, modifier)
    def decode_key(self, key_string):
        """Decode key_string into (key_code, modifier)"""
        modifier = 0
        the_key = None
        for key in key_string.split(' + '):
            mod = self.modifiers.get(key)
            if mod:
                modifier |= mod
            else:
                the_key = self.display.keysym_to_keycode(XK.string_to_keysym(key))
        return (the_key, self.escape_lock_mask(modifier))
    def escape_lock_mask(self, modifier):
        """
        Mask off numlock_mask, capslock_mask and scrolllock_mask. Otherwise
        you may find your key doesn't work only because the capslock(or anything)
        is open. That's not what we needed, so we mask it off.
        """
        return modifier & ~(self.scrolllock_mask | \
                               self.capslock_mask | \
                               self.numlock_mask)
    def grab_key(self, key_code, modifier):
        """
        Grab key press event of key_code+modifier.
        This will enable the X to send us a KeyPress event when the
        key is pressed.
        """
        self.do_grab_key(key_code, modifier)
        if self.numlock_mask:
            self.do_grab_key(key_code, modifier|self.numlock_mask)
        if self.capslock_mask:
            self.do_grab_key(key_code, modifier|self.capslock_mask)
        if self.scrolllock_mask:
            self.do_grab_key(key_code, modifier|self.scrolllock_mask)
        if self.numlock_mask and self.capslock_mask:
            self.do_grab_key(key_code, modifier|self.capslock_mask \
                                 |self.numlock_mask)
        if self.numlock_mask and self.scrolllock_mask:
            self.do_grab_key(key_code, modifier|self.scrolllock_mask \
                                 |self.numlock_mask)
        if self.scrolllock_mask and self.capslock_mask:
            self.do_grab_key(key_code, modifier|self.capslock_mask \
                                 |self.scrolllock_mask)
        if self.numlock_mask and \
                self.capslock_mask and self.scrolllock_mask:
            self.do_grab_key(key_code, modifier|self.capslock_mask \
                                 | self.numlock_mask | self.scrolllock_mask)
    def do_grab_key(self, key_code, modifier):
        """Helper function of grab_key."""
        self.root.grab_key(key_code, modifier, False, X.GrabModeAsync, X.GrabModeAsync)
    def event_loop(self):
        """Start an infinity loop and handle the incoming events."""
        self.root.change_attributes(event_mask=X.KeyPressMask)
        while 1:
            event = self.display.next_event()
            if event.type == X.KeyPress:
                self.handle_key_press(event)
            elif event.type == X.VisibilityNotify:
                self.handle_visibility_notify(event)
    def handle_key_press(self, event):
        """Key pressed means we will Jump or Exec now."""
        key_code = event.detail
        modifier = self.escape_lock_mask(event.state)
        job = self.jobs.get((key_code, modifier))
        if job:
            wind = job.search_existing(self.display, self.root)
            if wind:
                self.raise_window(wind)
            else:
                job.start_new()
    def raise_window(self, wind):
        """
        Raise the window.

        If it is not in iconic state, we just raise it and
        give it input focus. But if it is in iconic state,
        we have to invoke map to show it first. And since
        map doesn't show the window immediately, we can not
        set the focus immediately (setting the focus to a
        window not viewable have no effect). What we can do
        is to record the window(class name) and start
        listening the visibility notify event of this window,
        when the event arrive and the window(class name)
        matches, we set focus to it and remove the listener.
        """
        if wind.get_wm_state()['state'] == Xutil.IconicState:
            wind.map()
            # install a event listener
            wind.change_attributes(event_mask=X.VisibilityChangeMask)
            self.to_be_focus = ("%s.%s" % wind.get_wm_class())
        else:
            self.raise_and_focus_window(wind)
    def raise_and_focus_window(self, wind):
        wind.configure(stack_mode=X.Above)
        wind.set_input_focus(X.RevertToNone, X.CurrentTime)

    def handle_visibility_notify(self, event):
        """
        Visibility notify normally can only received from the window
        we will give focus. But to avoid accidently received event
        from other window(e.g. the user switchs to one another window
        very very very quickly), we compare the window class name
        before giving it focus.
        """
        wind = event.window
        if ("%s.%s" % wind.get_wm_class()) == self.to_be_focus:
            self.raise_and_focus_window(wind)
            self.to_be_focus = None
            # uninstall the listener
            wind.change_attributes(event_mask=0)

class Job:
    """
    A Job is consist of a Matcher and a Command. A Matcher is
    an object of the class Matcher, which is used to match against
    the window name and window class name to search for existing
    window. A Command is just a string, which is invoked to start
    a new process if no existing window is found.
    """
    def __init__(self, matcher, command):
        self.matcher = matcher
        self.command = command
    def search_existing(self, disp, root):
        tasks = root.get_full_property(
            disp.intern_atom("_NET_CLIENT_LIST"), Xatom.WINDOW).value
        for task in tasks:
            wind = disp.create_resource_object("window", task)
            if self.matcher.match(wind):
                return wind
        return None
    def start_new(self):
        subprocess.Popen(self.command, shell=True)

class Matcher:
    """
    Matcher is used to match against window. The window name and window
    class name is to be matched use the regexp given.
    """
    def __init__(self, wm_name = None, wm_class = None):
        self.wm_name = re.compile(wm_name or ".*")
        self.wm_class = re.compile(wm_class or ".*")
    def match(self, window):
        return self.wm_name.search(window.get_wm_name()) and \
            self.wm_class.search("%s.%s" % window.get_wm_class())

if __name__ == "__main__":
    jobs = [
    ("Mod4 + t", Job(Matcher(wm_class="urxvt.URxvt"), "urxvt")),
    ("Mod4 + f", Job(Matcher(wm_class="Navigator.Firefox"), "firefox")),
    ("Mod4 + v", Job(Matcher(wm_class="gvim.Gvim"), "gvim")),
    ("Mod4 + e", Job(Matcher(wm_name="emacs@.*"), "/home/kid/bin/emacs -f server-start")),
    ]
    joe = JumpOrExec()
    for job in jobs:
        joe.install_job(job[0], job[1])
    joe.event_loop()

