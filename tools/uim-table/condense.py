#!/bin/env python
# vim: set fileencoding=utf-8

import re
import os
import sys
import codecs
import tempfile

UIM_DEFAULT_ZM_TABLE = "/usr/share/uim/tables/zm.table"

# 要替换以及不需要自动处理的行
replaces = {
        '"':    u'("“”")',
        '(':    u'("(" "（")',
        ')':    u'(")" "）")',
        '=':    u'("=" "＝")',
        '^':    u'("^" "＾")',
        '_':    u'("_" "＿")',
        '#':    u'("#" "＃")',
        '.':    u'("。" ".")',
        '+':    u'("+" "＋")',
        '*':    u'("*" "＊")',
        '/':    u'("/" "／")',
        '\\':    u'("\\\\" "、" "＼")',
        '@':    u'("@" "＠")',
        '`':    u'("`" "‘")',
        '\'':    u'("\'" "’")',
        'mzs':  u'("么" "私")',
        'mazy': u'("每" "缷")',
        }

def get_traditional_chars_list():
    res = []
    ss = set(codecs.open(UIM_DEFAULT_ZM_TABLE, "r", "utf-8").read())
    for char in ss:
        try:
            char.encode('gb2312')
        except:
            res.append(char)

    return res

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print "Usage: convert.py out-file"
        sys.exit(1)

    outfile = sys.argv[1]

    omits = set(get_traditional_chars_list())

    print "Get %d traditional characters" % len(omits)

    count = 0

    res = []
    for line in codecs.open(UIM_DEFAULT_ZM_TABLE, "r", "utf-8"):
        chars = []
        for c in list(line):
            if c not in omits:
                chars.append(c)
        line = ''.join(chars)

        code, _ = line.split(None, 1)

        if code in replaces:
            res.append("%s %s\n" % (code, replaces[code]))
            continue

        # 删除繁体字符和词组后，还会遗留中间没有内容的一对空引号，需要删除它们。
        line = re.sub(r'\s*""', '', line)

        # 删除词组
        line = re.sub(r'\s*"[^\s]{2,}?"', '', line)

        # 清理空格
        line = re.sub(r'\(\s', '(', line)

        # 如果当前行的所有中文字符都被删除，那么整行都可以丢弃了。
        if not re.search(r'\(\)', line):
            res.append(line)

    # 从码表上去除自动上屏
    final = []
    for i in range(len(res)):
        code1, char1 = res[i].split(None, 1)
        if i == len(res) - 1:
            code2 = 'z'
        else:
            code2, char2 = res[i+1].split(None, 1)
        final.append(res[i])
        if (code1.isalpha()
                and code2.isalpha()
                and len(code1) < 4
                and not code2.startswith(code1)):
            final.append(code1 + "v" * (4 - len(code1)) + ' ' + char1)
            
    out = codecs.open(outfile, "w", "utf-8")
    out.write(''.join(final))
    out.close()
