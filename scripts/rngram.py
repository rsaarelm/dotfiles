#!/usr/bin/env python

"""
Grammar-based random string generator.

Based on the Pyro script described in the book Mind Performance Hacks:
http://www.ludism.org/mentat/MindPerformanceHacks_2fForceYourConnections

TODO: Use capitalized indexers to produce capitalized substrings.

Example rngr input file:

---
root_item
  option1 %(sub1)s
  option2

sub1
  subitem
  %(sub2)s %(sub2)s

sub2
  foo
  bar
---
"""

import random
import re
import sys

valid_name = re.compile(r"[_a-zA-Z]\w*")

def is_empty(str):
    return len(str.strip()) == 0

def indent_level(str):
    result = 0
    for x in str:
        if x.isspace():
            result += 1
        else:
            break
    return result

def recursive_format(str, fmt):
    result = str % fmt
    result2 = result % fmt
    while result2 != result:
        result = result2
        result2 = result % fmt
    return result

class ChoiceDict:
    def __init__(self, data):
        self.data = data
        self.seed = None

    def __getitem__(self, name):
        return random.choice(self.data[name])

    def generate(self, seed=None):
        if not seed:
            seed = self.seed
        return recursive_format(self[seed], self)

def build_dict(lines):
    result = {}
    current = None
    line_num = 0
    seed = None
    for line in lines:
        line_num += 1

        if is_empty(line):
            continue

        indent = indent_level(line)

        line = line.strip()

        # current item has been processed, move to the next one
        if current and indent == 0:
            current = None

        if not current:
            if not valid_name.match(line):
                raise "Invalid element name %s on line %d." % (line, line_num)
            if indent != 0:
                raise "Confused by indentation on line %d." % line_num
            current = line
            if not seed:
                seed = current
            if not current in result:
                result[current] = []
        else:
            if indent == 0:
                raise "Confused by indentation on line %d." % line_num
            result[current].append(line)

    obj = ChoiceDict(result)
    obj.seed = seed
    return obj

def usage():
    print "Usage:"
    print "%s [grammar file] {number of items to generate}" % sys.argv[0]

def main(argv):
    if len(argv) < 2:
        usage()
        return
    file = open(argv[1])
    n = 1

    if len(argv) > 2:
        n = int(argv[2])

    dict = build_dict(file.readlines())

    for i in range(n):
        print dict.generate()

if __name__ == '__main__':
    main(sys.argv)
