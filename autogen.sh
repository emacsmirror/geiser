#!/bin/sh

[ -f elisp/geiser.el ] || exit 1

autoreconf -Wall -i
