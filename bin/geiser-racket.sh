#!/bin/bash
#|
top="$(dirname $0)/../scheme"
exec racket -i -S "$top/racket" -l errortrace -cu "$0" ${1+"$@"}
|#

#lang racket/base

(require (lib "cmdline.rkt"))

(define port (make-parameter 1969))

(command-line
 "run-racket.sh" (current-command-line-arguments)
 (once-each
  (("-p" "--port") p "Geiser server port" (port (string->number p)))))

(and ((dynamic-require 'geiser/server 'start-geiser) (port))
     (printf "Geiser server running at port ~a~%" (port)))
