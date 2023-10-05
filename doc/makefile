# Copyright (C) 2010, 2020, 2021, 2022, 2023 Jose Antonio Ortega Ruiz
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
	rm -f *.info

web:
	rm -f $(output_dir)/*.html
	texi2any --output=$(output_dir) \
	  --html \
	  --css-ref=geiser.css \
	  --no-headers \
	  --no-number-sections \
	  --no-node-files \
	  --split=chapter \
	  $(CURDIR)/geiser.texi && \
	cp -r $(CURDIR)/img ${output_dir}

http: web
	cd $(output_dir) && python -m http.server $(port)

info: geiser.info dir

%.info: %.texi
	makeinfo --no-split $< -o $@
