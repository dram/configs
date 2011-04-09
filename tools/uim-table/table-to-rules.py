#!/usr/bin/env python

import sys
import codecs

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print "Usage: table-to-rules.py infile outfile"
        sys.exit(1)

    infile = sys.argv[1]
    outfile = sys.argv[2]

    out = codecs.open(outfile, "w", "utf-8")

    out.write("(define zhengma-rule '(\n")
    for line in codecs.open(infile, "r", "utf-8"):
        rule, value = line.strip().split(None, 1)
        escaped = [c.replace('\\', '\\\\').replace('"', '\\"') for c in rule]
        new = "((%s))" % " ".join(['"%s"' % c for c in escaped])
        out.write("(%s %s)\n" % (new, value))
    out.write("))\n")

    out.close()
