/*
 * File: fake-key.c
 * Author: Xin Wang <dram.wang@gmail.com>
 * Desc:
 * 	Simulate X11 key press and release event. Pass keycode as arguments.
 *	Keycode can be grabbed from `xev | grep keycode`.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/extensions/XTest.h>

int main(int argc, char *argv[])
{
	int i;

	if (argc <= 1) {
		fprintf(stderr, "usage: fake-key key1 key2 key3 ...\n");
		return 1;
	}
		
	Display *dpy = XOpenDisplay(NULL);

	if (!dpy)
		return 1;

	for (i = 1; i < argc; ++i)
		XTestFakeKeyEvent(dpy, atoi(argv[i]), True, CurrentTime);

	for (i = argc - 1; i > 0; --i)
		XTestFakeKeyEvent(dpy, atoi(argv[i]), False, CurrentTime);

	XSync(dpy, False);

	XCloseDisplay(dpy);

	return 0;
}
