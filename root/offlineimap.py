#!/usr/bin/env python

import re
from subprocess import check_output


def get_password_emacs(machine, login, port):
    s = 'machine %s login %s port %s password ([^\n ]*)' % (machine, login, port)
    p = re.compile(s)
    b = check_output('gpg -q --no-tty -d ~/.authinfo.gpg', shell=True)
    authinfo = b.decode()
    return p.search(authinfo).group(1)
