elpa_name = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)
elpa_dir = $(elpa_name)
elpa_slogan = "GNU Emacs and Scheme talk to each other"
geiser_el = $(elpa_dir)/geiser.el
bin_dir = $(elpa_dir)/bin
scheme_dir = $(elpa_dir)/scheme

elpa:  info
	rm -rf $(elpa_dir)
	mkdir -p $(elpa_dir)

	echo '(define-package "geiser" "$(PACKAGE_VERSION)" $(elpa_slogan))' \
             > $(elpa_dir)/geiser-pkg.el

	cp $(abs_top_srcdir)/elisp/*.el $(elpa_dir)

	cp doc/geiser.info $(elpa_dir)
	(cd $(elpa_dir) && install-info --dir=dir geiser.info 2>/dev/null)

	cp readme.org $(elpa_dir)/README

	rm -f $(elpa_name).tar
	tar cf $(elpa_name).tar $(elpa_name)
