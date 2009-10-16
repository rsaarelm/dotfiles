#!/usr/bin/env python
"""Print a status bar to dzen.

Example invocation, in .xinitrc:

tail -f ~/.my_message_pipe | status_bar.py | dzen2 -e '' -ta r &
"""

import time
import sys
import os
import threading

DELAY_SECS = 1.0

CULL_AGE = 60

# Timestamped list of existing messages
msg_queue = []

msg_lock = threading.Lock()

def add_message(msg):
  try:
    msg_lock.acquire()
    msg_queue.append((msg.strip() + ' ', time.time()))
  finally:
    msg_lock.release()

def age_color(age):
  decay_sec = 10.0
#  t = 1.0 / (1.0 + abs(age) / decay_sec)
  t = abs(age) / decay_sec

  def ramp(t):
    if t < 0.0: return 0.0
    elif t < 1.0: return t
    elif t < 2.0: return 1.0
    elif t < 3.0: return 3.0 - t
    else: return 0.0

  r = ramp(t + 1.0)
  g = ramp(t) / 2.0
  b = ramp(t - 1.0)

#  shade = int(255 * 1.0 / (1.0 + (age / 10.0)))
  return "#" + "".join(["%02x" % int(255 * x) for x in (r, g, b)])

def cull_messages():
  global msg_queue
  now = time.time()
  msg_queue = [(msg, tstamp) for (msg, tstamp) in msg_queue if now - tstamp < CULL_AGE]

class StdinReader(threading.Thread):
  def run(self):
    while True:
      add_message(sys.stdin.readline())

def msg_str((msg, tstamp)):
  return "^fg(%s)%s^fg()" % (age_color(time.time() - tstamp), msg)

def accumulate_msgs(acc, msgs, space_left):
  if len(msgs) == 0: return acc
  msg = msgs[-1]
  if space_left >= len(msg[0]):
    acc.insert(0, msg_str(msg))
    return accumulate_msgs(acc, msgs[:-1], space_left - len(msg[0]))
  elif space_left <= 0:
    return acc
  else:
    acc.insert(0, msg_str((msg[0][-space_left:], msg[1])))
    return acc

def main(*argv):
  global msg_queue
  StdinReader().start()
  while True:
    string = ""

    try:
      msg_lock.acquire()

      cull_messages()

      # Show the last n characters worth of stuff from the message queue
      # colored appropriately.
      string = "".join(accumulate_msgs([], msg_queue, 120))

    finally:
      msg_lock.release()

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
