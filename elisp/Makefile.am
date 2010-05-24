EXTRA_DIST = geiser-install.el.in

dist_lisp_LISP = \
   geiser-autodoc.el \
   geiser-base.el \
   geiser-company.el \
   geiser-compile.el \
   geiser-completion.el	\
   geiser-connection.el	\
   geiser-custom.el \
   geiser-debug.el \
   geiser-doc.el \
   geiser-edit.el \
   geiser.el \
   geiser-eval.el \
   geiser-guile.el \
   geiser-impl.el \
   geiser-log.el \
   geiser-mode.el \
   geiser-racket.el \
   geiser-popup.el \
   geiser-reload.el \
   geiser-repl.el \
   geiser-syntax.el \
   geiser-xref.el \
   geiser-version.el

lisp_LISP = geiser-install.el

CLEANFILES = geiser-install.el

geiser-install.el: $(srcdir)/geiser.el $(srcdir)/geiser-install.el.in
	@sed -e "s|@SCHEME_DIR[@]|$(datarootdir)/geiser|"  $(srcdir)/geiser-install.el.in >$@

