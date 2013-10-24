Homegit is a system for managing configuration files in your home
directory using Git. The idea is from Christian Neukirchen:
http://chneukirchen.org/blog/archive/2013/01/a-grab-bag-of-git-tricks.html

To prevent interference with other Git projects under the home
directory, the homegit repository does not live at ~/.git and is
accessed using a special 'homegit' alias that replaces the 'git'
command.

To use, do `git clone --bare` of homegit into $HOMEGIT_PATH and
use `alias homegit="GIT_DIR=$HOMEGIT_PATH GIT_WORK_TREE=~ git"`.

