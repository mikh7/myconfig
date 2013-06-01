#!/bin/sh

#
# TODO: put some kind of resolve symlink functions into library
# and use them
# 

for f in ./* ; do 
  # is it initially a link?
  if [ -h "$f" -a -f "$f" ] ; then
    target="$f"
    while [ -h "$target" -a -f "$target" ] ; do
      target=`readlink "$target"`
    done
    if [ -f "$target" ] ; then
      if ! ln "target" tmp$$ 2> /dev/null ; then
        cp "$target" tmp$$
      fi
      if [ $? != 0 ] ; then
        exit $?
      fi
      mv tmp$$ "$f"
      if [ $? != 0 ] ; then
        exit $?
      fi
    fi
  fi
done
