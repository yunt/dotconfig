#!/bin/sh

# Time-stamp: <01/10/2008 17:44:51 星期四 by ahei>

set -x
aclocal                                         \
&& autoconf                                     \
&& libtoolize --copy --force --automake         \
&& automake --foreign --add-missing
