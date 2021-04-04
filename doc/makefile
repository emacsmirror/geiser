# Copyright (C) 2010, 2020, 2021 Jose Antonio Ortega Ruiz
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

top_srcdir=..
output_dir=html
port ?= 8082

clean:
	rm -f $(output_dir)/*.html

web:
	rm -f $(output_dir)/*.html
	texi2html --output=$(output_dir) \
                  --split=chapter \
                  --noheader \
                  --nonumber-section \
                  --init-file=$(top_srcdir)/doc/site.conf \
                  --top-file=index.html \
                  $(top_srcdir)/doc/web.texi
	cp $(top_srcdir)/doc/geiser.css ${output_dir}
	cp -r $(top_srcdir)/doc/img ${output_dir}

http: web
	cd $(output_dir) && python -m http.server $(port)
