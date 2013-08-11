SUBDIRS = . elisp scheme bin doc

EXTRA_DIST = THANKS

dist-hook:
	$(SHELL) $(top_srcdir)/ChangeLog > $(top_distdir)/ChangeLog

elpa_name = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)
elpa_dir = $(abs_top_builddir)/$(elpa_name)
elpa_slogan = "GNU Emacs and Scheme talk to each other"
geiser_el = $(elpa_dir)/geiser.el

elpa: README.elpa info
	rm -rf $(elpa_dir)
	$(mkdir_p) $(elpa_dir)

	echo '(define-package "geiser" "$(PACKAGE_VERSION)" $(elpa_slogan))' \
             > $(elpa_dir)/geiser-pkg.el

	$(INSTALL_DATA) $(abs_top_srcdir)/elisp/*.el \
                        $(top_builddir)/elisp/geiser-version.el \
                        $(elpa_dir)
	sed -i -e 's|(expand-file-name "../scheme/" .*$$|geiser-elisp-dir|' \
                  $(geiser_el)
	sed -i -e 's/(autoload /;;;###autoload\n(autoload /' $(geiser_el)

	$(mkdir_p) $(elpa_dir)/guile/geiser
	$(mkdir_p) $(elpa_dir)/racket/geiser
	$(INSTALL_DATA) $(abs_top_srcdir)/scheme/guile/geiser/* \
                        $(elpa_dir)/guile/geiser
	$(INSTALL_DATA) $(abs_top_srcdir)/scheme/racket/geiser/* \
                        $(elpa_dir)/racket/geiser

	$(INSTALL_DATA) $(srcdir)/doc/geiser.info $(elpa_dir)
	(cd $(elpa_dir) && install-info --dir=dir geiser.info 2>/dev/null)

	$(INSTALL_DATA) $(top_srcdir)/README.elpa $(elpa_dir)/README

	$(mkdir_p) $(elpa_dir)/bin
	$(INSTALL) $(top_srcdir)/bin/geiser-racket.sh $(elpa_dir)/bin/geiser-racket
	sed -i -e  's|^top=.*$$|top=$$(dirname $$0)/..|' \
                   $(elpa_dir)/bin/geiser-racket

	rm -f $(elpa_name).tar
	tar cf $(elpa_name).tar $(elpa_name)

