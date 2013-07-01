# Copyright (C) 2009, 2010, 2011, 2012, 2013 Jose A Ortega Ruiz.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT([Geiser],[0.5],[geiser-users@nongnu.org],geiser)
AC_CONFIG_SRCDIR([elisp/geiser.el])
AM_INIT_AUTOMAKE

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo, no)

AM_PATH_LISPDIR

AC_CONFIG_FILES([
Makefile
elisp/Makefile
elisp/geiser-version.el
elisp/geiser-load.el
scheme/Makefile
bin/Makefile
doc/Makefile
])

AC_OUTPUT
