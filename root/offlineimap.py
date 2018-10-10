#!/usr/bin/env python

from subprocess import check_output


def get_password(account):
    key = 'Mail/' + account
    return check_output(['pass', key]).decode('utf-8').strip()
