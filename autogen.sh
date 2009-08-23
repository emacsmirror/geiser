#!/bin/sh

[ -f elisp/geiser.el ] || exit 1

touch ./ChangeLog

autoreconf -Wall

rm ./ChangeLog
