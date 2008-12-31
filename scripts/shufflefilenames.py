#!/usr/bin/env python

"""Alter the names of the files in the current directory so that they form a
random lexical permutation.

The files are stripped of any numerical prefixes ("^[0-9]+") they might have,
and assigned a set of random numerical prefixes of equal length. This will
make the lexical order of the file names random. A file name consisting of
only zeroes up to a period will not be altered, except by being padded with
more zeroes if the sequence of zeroes is shorter than the length of the
prefix. Files with names consisting of only zeroes are assumed to contain
special data for the beginning of the folder.

This script was written for a digital audio player which does not support
shuffle play within a folder. The script will effectively create a one-time
shuffled playlist of the contents of the directory.

The script is currently hardcoded to only shuffle mp3 files via the
ALLOWED_SUFFIXES variable.

WARNING: This script can do damage to your file system. By default, it'll only
mess with audio files, and it will only permanently mangle their prefix
numbers. Still, it does perform a theoretically irreversible directory-wide
operation.

"""

import random
import sys
import os
import os.path
import re
import math

ONLY_ALLOWED_SUFFIXES = True
ALLOWED_SUFFIXES = [
  '.mp3',
  '.wma',
  '.ogg',
  '.flac',
  ]

def is_valid_name(filename):
  if ONLY_ALLOWED_SUFFIXES:
    for suf in ALLOWED_SUFFIXES:
      if filename.endswith(suf):
        return True
      return False
  else:
    return True

def is_special_file(filename):
  return re.match(r"^0+\.", filename)

def preprocess(filename):
  if is_special_file(filename):
    return filename
  else:
    return re.sub("^[0-9]+", "", filename)

def main(path):
  print "Shuffling names in '%s'" % path
  filenames = [name for name in os.listdir(path) if is_valid_name(name)]
  new_names = [preprocess(name) for name in filenames]
  pad = int(math.ceil(math.log(len(filenames) + 1) / math.log(10))) + 1
  formatstr = "%%0%dd" % pad

  prefixes = range(1, len(filenames) + 1)
  random.shuffle(prefixes)
    
  for (i, name) in enumerate(new_names):
    if not is_special_file(name):
      new_names[i] = (formatstr % prefixes[i]) + new_names[i]
      for j in range(96):
        if new_names[i] != filenames[i] and os.path.exists(new_names[i]):
          new_names[i] = '0' + new_names[i]
        else:
          break
      if new_names[i] != filenames[i] and os.path.exists(new_names[i]):
        print "Too many name clashes, exiting."
        return

  for (old, new) in zip(filenames, new_names):
    if old == new:
      continue
    assert not os.path.exists(new)
    os.rename(os.path.join(path, old), os.path.join(path, new))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        path = sys.argv[1]
    else:
        path = "."
    main(path)
