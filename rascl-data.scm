;;; rascl-data.scm -- A data structure for RASCL configurations.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module rascl-data
        *
        (import scheme chicken)
        (use matchable)

(define (rdata-ref rdata key)
  (match key
    [() (error "Invalid key.")]
    [(k) (alist-ref k rdata)] 
    [(k . sk)
     (rdata-ref (alist-ref k rdata) sk)]
    [k (alist-ref k rdata)]))

(define (rdata-update rdata key value)
  #f)

(define (rdata-insert rdata key value position)
  #f)

) ; END MODULE
