#!/usr/bin/env python
"""Print a status bar to dzen."""

import time
import sys

DELAY_SECS = 10.0

def main(*argv):
  while True:
    string = ""
    # XXX: strftime week numbers start from zero, while the ones a real
    # calendar uses start from 1. Need to kludge a bit to get the correct
    # number.
    weeknum = int(time.strftime("%W")) + 1
    string += time.strftime("%a %Y-%m-%d (week %%s) %H:%M") % weeknum
    print string
    sys.stdout.flush()
    time.sleep(DELAY_SECS)

if __name__=='__main__':
  main(*sys.argv)
