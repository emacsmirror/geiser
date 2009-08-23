# Copyright (C) 2009 Free Software Foundation, Inc.
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

AC_INIT([GNU Geiser],[0.0.7],[jao@gnu.org],geiser)
AC_CONFIG_SRCDIR([elisp/geiser.el])
AM_INIT_AUTOMAKE

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo, no)
AC_CHECK_PROG(TEXI2PDF, texi2pdf, texi2pdf, false)

AM_PATH_LISPDIR

dnl scheme/plt/Makefile
dnl scheme/plt/geiser/Makefile

AC_CONFIG_FILES([
Makefile
elisp/Makefile
elisp/geiser-version.el
scheme/Makefile
])

AC_OUTPUT
