#!/usr/bin/env python

"""Shows a timer on the command line.

Used for project work, start the timer and keep it running for at least a
given time until stopping work. """

from time import *
import sys
import os
import pickle

TIME_FORMAT = "%Y-%m-%d %H:%M:%S"

SHOW_BREAK_END_MSG = True
BREAK_END_CMD = "xmessage Break is over"

# Untested stuff: Windows kbhit, adjusting today's time for tasks that go
# over midnight.

try:
  # If we're on Windows, we might get a ready-made kbhit.
  from msvcrt import kbhit
except ImportError:
  # Define a kbhit under unix, from Python FAQ.
  import termios, os, fcntl
  def kbhit():
    fd = sys.stdin.fileno()
    old = termios.tcgetattr(fd)
    new = termios.tcgetattr(fd)
    new[3] = new[3] & ~termios.ICANON & ~termios.ECHO
    termios.tcsetattr(fd, termios.TCSANOW, new)
    termios.tcsetattr(fd, termios.TCSADRAIN, new)

    oldflags = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, oldflags | os.O_NONBLOCK)

    try:
      try:
        sys.stdin.read(1)
        return True
      except IOError:
        return False
    finally:
      termios.tcsetattr(fd, termios.TCSADRAIN, old)
      termios.tcsetattr(fd, termios.TCSAFLUSH, old)
      fcntl.fcntl(fd, fcntl.F_SETFL, oldflags)

def time_str(seconds):
  return "%02d:%02d:%02d" % (seconds / 3600, seconds / 60 % 60, seconds % 60)

def print_time(seconds, today, total):
  print ("\rTime: %s\tToday: %s\tTotal: %s         " % 
    (time_str(seconds), time_str(seconds + today), time_str(seconds + total))),
  sys.stdout.flush()

def day_interval(second_within_day):
  # Zero time
  time = [0] * 9
  # Set date.
  time[:3] = localtime(second_within_day)[:3]
  day_start = mktime(time)
  day_end = day_start + 86400
  return (day_start, day_end)

class Records:
  def __init__(self):
    # Data is a list of (start_secs, duration_secs, message_string) tuples
    self._data = []

  def save(self, file):
    for start, duration, msg in self._data:
      assert '\n' not in msg
      print >> file, "%s|%s|%s" % (strftime(TIME_FORMAT, localtime(start)), duration, msg)

  def load(self, file):
    self._data = []
    for line in file:
      line = line.strip() # Get rid of trailing newline
      start, duration, msg = line.split('|', 2)
      start = mktime(strptime(start, TIME_FORMAT))
      duration = int(duration)
      self.add_entry(start, duration, msg)

  def add_entry(self, start, duration, msg):
    if duration < 1:
      return
    assert('\n' not in msg)
    msg = msg.strip()
    self._data.append((int(start), int(duration), msg))

  def total_time(self):
    return sum([duration for (_, duration, _) in self._data])

  def time_in_interval(self, min_sec, max_sec):
    result = 0
    for start, duration, _ in self._data:
      if start >= min_sec and start + duration < max_sec:
        result += duration
      elif start >= min_sec:
        result += max(0, min(start + duration, max_sec) - start)
      elif start + duration < max_sec:
        result += max(0, start + duration - min_sec)
    return result

  def time_for_day(self, second_within_day):
    day_start, day_end = day_interval(second_within_day)
    return self.time_in_interval(day_start, day_end)

  def time_for_today(self):
    return self.time_for_day(time())

  def daily_hours(self):
    result = {}
    for start, duration, msg in self._data:
      day_start, day_end = day_interval(start)
      day_start = localtime(day_start)
      if day_start not in result:
        result[day_start] = self.time_for_day(start)
    result = result.items()
    result.sort()
    return result
        
MODE_TIMER = 1
MODE_REPORT = 2

def parse_time(time_str):
  if time_str.endswith('h'):
    return int(float(time_str[:-1]) * 3600)
  elif time_str.endswith('m'):
    return int(float(time_str[:-1]) * 60)
  # Allow 's' suffix for completeness' sake
  elif time_str.endswith('s'):
    time_str = time_str[:-1]
  
  return int(float(time_str))

def usage():
  print "Usage: %s [options] [file]" % sys.argv[0]
  print "options: -r        print report of log file"
  print "         -h        print this help"
  print "         -c [time] count down for [time] seconds before starting"
  print "                   use [time]m for minutes and [time]h for hours"

def countdown(seconds):
  """Show a countdown on screen. Stop when the user presses a key or when time
  runs out."""

  end_time = time() + seconds

  while not kbhit() and time() < end_time:
    seconds = end_time - time()
    print "\rBreak left: %s         " % time_str(seconds),
    sys.stdout.flush()
    sleep(0.1)
  print "\r                       ",

def timer(records, filename=None):
  """Show a timer on screen until the user presses a key. Record the amount of
  time elapsed."""

  begin = time()
  total = records.total_time()
  today = records.time_for_today()
  # Use adjust_today when crossing midnight and today's total is actually less than
  # session total.
  adjust_today = 0
  seconds = 0

  DATE_FORMAT = "%Y%m%d"
  date = strftime(DATE_FORMAT)
  while not kbhit():
    try:
      seconds = int(time() - begin)
      print_time(seconds, today - adjust_today, total)
      sleep(0.1)
      newdate = strftime(DATE_FORMAT)
      # The day has changed. Save last day's time.
      if newdate != date:
        adjust_today += seconds - adjust_today
        date = newdate
    except KeyboardInterrupt:
      print "\nSpatiotemporal anomaly detected. Memory of the current session will be purged."
      return

  if filename:
    entry = raw_input("\nLog entry: ")

    records.add_entry(begin, seconds, entry)

    file = open(filename, 'wb')
    records.save(file)
    file.close()

def report(records):
  days = records.daily_hours()
  print "Date\t\tHours"
  for date, seconds in days:
    print "%s\t%.3f" % (strftime("%Y-%m-%d", date), (seconds / 3600.0))

  # Sort days by seconds for median.
  daily_hours = [seconds / 3600.0 for (date, seconds) in days]
  daily_hours.sort()
  if len(daily_hours) % 2 == 1:
    median_hours = daily_hours[len(daily_hours) / 2]
  else:
    # If there is an even number of entries, interpolate between the two
    # middle entries.
    median_hours = (daily_hours[len(daily_hours) / 2] / 2 +
      daily_hours[len(daily_hours) / 2 - 1] / 2)

  total_hours = records.total_time() / 3600.0
  print "-" * 32
  print
  print "Daily mean:    %.3f h" % (total_hours / len(days))
  print "Daily median:  %.3f h" % median_hours
  print "Total hours:   %.3f h" % total_hours

  todays_time = records.time_for_today()
  if todays_time > 0:
    print "Today's time: ", time_str(todays_time)

def main():
  records = Records()

  filename = None
  countdown_secs = 0

  mode = MODE_TIMER

  # Need to use a crude loop since we can manipulate i from within it.
  i = 1
  while i < len(sys.argv):
    param = sys.argv[i]

    if param.startswith('-'):
      if param == '-r':
        mode = MODE_REPORT
      elif param == '-h':
        usage()
        return
      elif param == '-c':
        try:
          i += 1
          countdown_secs = parse_time(sys.argv[i])
          assert countdown_secs >= 0
        except:
          usage()
          return
      else:
        print "Unknown option '%s'" % param
        usage()
        return 1
    else:
      if filename is None:
        filename = param

    i += 1
  
  if filename is not None:
    if os.path.exists(filename):
      file = open(filename, 'rb')
      records.load(file)
      file.close()

  if mode == MODE_TIMER:
    if countdown_secs > 0:
      countdown(countdown_secs)
      if SHOW_BREAK_END_MSG:  
        os.system(BREAK_END_CMD)
    timer(records, filename)
  elif mode == MODE_REPORT:
    if filename is None:
      print "No file to generate report from."
      return 1
    else:
      report(records)

if __name__ == '__main__':
  main()
