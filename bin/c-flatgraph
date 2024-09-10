#!/usr/bin/env python3

# Simple dependency analyzer for C/C++ projects that relies on include
# patterns.

# Uses gcc -M
#
# Put all necessary include path dependencies (-I[path]) in $CFLAGS before
# running

# Handy CFLAGS generation command:
#     for dir in `find -name '*.hpp' -or -name '*.h' -printf '%h\n' | sort -u`; do export CFLAGS="$CFLAGS -I$dir"; done

from collections import defaultdict
import functools
import glob
import mimetypes
import os
import os.path
import re
import subprocess
import sys

def module(path):
    # XXX: This won't work right if headers are in a different directory from
    # sources. Maybe it can be patched around by removing "include/", "src/"
    # or "source/" fragments from the path as well, but this depends on the
    # structure of the project you're analyzing.
    return os.path.splitext(path)[0]

def read_deps(path):
    try:
        text = subprocess.check_output("gcc $CFLAGS -M %s" % path, shell=True)
    except subprocess.CalledProcessError as e:
        return []
    ret = []
    try:
        text = text.decode('utf-8')
    except:
        text = text.decode('ascii')
    for line in text.splitlines():
        # First line is for the benefit of Makefiles, "foo.o: foo.cpp \".
        # It has no info we need, so throw it away.
        if ':' in line:
            continue
        # Get rid of the trailing backslashes, also for Makefiles
        if line.endswith('\\'): line = line[:-1]
        for elt in [p for p in line.strip().split()]:
            ret.append(elt)
    return ret

def main():
    files = []
    for arg in sys.argv[1:]:
        matches = glob.glob(arg)
        files.extend(matches)

    # Names that should match both sources and headers.
    modules = {module(x) for x in files}
    dep_graph = defaultdict(set)

    for f in files:
        dep_graph[module(f)] = set()
        deps = read_deps(f)
        for dep in deps:
            # Ignore self-refs.
            if module(dep) == module(f): continue
            # Ignore deps out of the input set.
            if module(dep) in modules:
                dep_graph[module(f)].add(module(dep))

    reading_list = []
    while dep_graph:
        # Find the module with least dependencies, unless there are circular
        # dependencies this should be zero.
        smallest = min(dep_graph, key=lambda k: len(dep_graph[k]))
        reading_list.append(next(x for x in files if module(x) == smallest))

        # Delete this module from the graph.
        del dep_graph[smallest]
        for m in list(dep_graph.keys()):
            dep_graph[m].discard(smallest)

    reading_list.reverse()
    for line in reading_list:
        print(line)

if __name__ == '__main__':
    main()
