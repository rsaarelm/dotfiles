#!/usr/bin/env python

"""Managing tags in file names.

The tag syntax is: PREFIX.TAG1.TAG2. ... .TAGN.SUFFIX
"""

import os
import os.path
import sys
import re

# Don't change. Changing the separator will also 
TAG_SEPARATOR = '.'

def clean_untagged_filename(path):
  """Removes all TAG_SEPARATORs, whitespace and other nasty stuff from a file name.

  Note that the file name must be untagged. Removing tag separators from
  tagged names ruins the tags."""

  path, filename = os.path.split(path)
  if not filename:
    raise ValueError("Path '%s' has no file." % path)
  body, ext = os.path.splitext(filename)

  # Make it entirely lowercase
  body = body.lower()
  # Convert TAG_SEPARATORs to underlines
  body = re.sub("\\" + TAG_SEPARATOR, "_", body)
  # Convert whitespace to underlines
  body = re.sub(r"\s+", "_", body)
  # Make hyphens tighter.
  body = re.sub(r"_-_", "-", body)

  return body + ext

def get_root_and_tail(filename):
  """Returns a tuple split at the first tag separator.

  The first part is the root filename."""

  idx = filename.find(TAG_SEPARATOR)
  if idx == -1:
    return (filename, "")
  else:
    return (filename[:idx], filename[idx:])

def get_tags(filename):
  """Return a list of tags in a name."""
  return filename.split(TAG_SEPARATOR)[1:-1]

def make_filename(prefix, tags, suffix):
  if len(tags) > 0:
    middle = TAG_SEPARATOR.join(tags)
    return TAG_SEPARATOR.join((prefix, middle, suffix))
  else:
    return TAG_SEPARATOR.join((prefix, suffix))

def set_tags(old_filename, tags):
  old_list = old_filename.split(TAG_SEPARATOR)

  if len(old_list) == 1:
    # No suffix, no tags
    if len(tags) == 0:
      # No tags to be added, just return the plain name.
      return old_list[0]
    else:
      # Some tags. Need to add a period to the end. make_filename will do
      # this.
      return make_filename(old_list[0], tags, '')
  else:
    prefix = old_list[0]
    suffix = old_list[-1]
    if len(suffix) == 0 and len(tags) == 0:
      # Empty suffix and no tags in the new version, remove all
      # periods
      return prefix
    else:
      return make_filename(prefix, tags, suffix)

def add_tags(filename, *tags):
  file_tags = get_tags(filename)
  for tag in tags:
    if tag not in file_tags:
      file_tags.append(tag)
  return set_tags(filename, file_tags)

def remove_tags(filename, *tags):
  file_tags = get_tags(filename)
  #print "File tags for", filename, file_tags
  for tag in tags:
    if tag in file_tags:
      file_tags.remove(tag)
  return set_tags(filename, file_tags)

def has_tag(filename, tag):
  return tag in get_tags(filename)

def do_rename(old_filename, new_filename):
  if old_filename == new_filename:
    return

  if not os.path.isfile(old_filename):
    raise ("Aborted: Source file "+old_filename+" does not exist.")

  if os.path.isfile(new_filename):
    raise ("Aborted: Target file "+new_filename+" already exists.")

  os.rename(old_filename, new_filename)

def cmd_line(args):
  def usage():
    print (
      "Usage: %s file [[add|remove] tag [tag2, tag3, ...] |\n"+
      "%s              replace old_tag new_tag]") % \
      (args[0], ' '*len(args[0]))

    sys.exit(1)

  if len(args) < 4: usage()

  filename = args[1]
  op = args[2]
  tags = args[3:]

  if op == 'add':
    do_rename(filename, add_tags(filename, *tags))
  elif op == 'remove':
    do_rename(filename, remove_tags(filename, *tags))
  elif op == 'replace':
    if len(args) != 5: usage()
    new_name = remove_tags(filename, args[3])
    new_name = add_tags(new_name, args[4])
    do_rename(filename, new_name)
  else:
    usage()

if __name__ == '__main__':
  args = sys.argv
  cmd_line(args)
