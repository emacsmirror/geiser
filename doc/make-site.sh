#!/bin/sh

out=html

rm ./${out}/*.html

texi2html --output=${out} \
    --split=chapter \
    --noheader \
    --nonumber-section \
    --init-file=./site.conf \
    --top-file=index.html \
    geiser.texi

cp geiser.css ${out}
