SUBDIRS = . elisp scheme

EXTRA_DIST = THANKS

dist-hook:
	git log --summary --stat > $(top_distdir)/ChangeLog
