EXTRA_DIST = geiser-racket.sh
dist_bin_SCRIPTS = geiser-racket
noinst_SCRIPTS = geiser-racket-noinst

CLEANFILES = $(dist_bin_SCRIPTS) $(noinst_SCRIPTS)

geiser-racket: $(srcdir)/geiser-racket.sh
	@sed -e "s|top=\".*\"|top=$(datarootdir)/geiser|" \
             $(srcdir)/geiser-racket.sh >$@
	@chmod +x $@

geiser-racket-noinst: $(srcdir)/geiser-racket.sh
	@sed -e "s|top=\".*\"|top=$(abs_top_srcdir)/scheme|" \
             $(srcdir)/geiser-racket.sh >$@
	@chmod +x $@
