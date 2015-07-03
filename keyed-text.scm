;;; keyed-text.scm -- Strings accessed by keys for RASCL.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module keyed-text
        *
        (import scheme chicken)
	(use matchable)
	(use srfi-13)

(define (make-ktext #!key (keys '()) (data ""))
  `(,keys ,data))

(define (ktext-size ktext)
  (match ktext
    [(_ data) (string-length data)]))

(define (get-sub key keys text)
  (let* ((key-data (alist-ref key keys))
         (idx (car key-data))
         (len (cadr key-data))
         (subkeys (cddr key-data)))
    (values
      (substring text idx (+ idx len))
      subkeys)))

(define (ktext-ref ktext key)
  (match `(,key ,ktext)
    [(() _) (error "Invalid key")]
    [((k) (keys data))
     (get-sub k keys data)]
    [((k . sk) (keys data))
     (let-values (((subtext subkeys) (get-sub k keys data)))
     	 (ktext-ref (make-ktext subkeys subtext) sk))]
    [(k (keys data))
     (get-sub k keys data)]))

(define (realign-keys keys start delta)
  (map
    (lambda (key-data)
      (if (>= (car key-data) start)
	`(,(+ (car key-data) delta) ,@(cdr key-data))
	key-data))
    keys))

(define (ktext-replace ktext key value)
  (let ((new-len
	  (string-length value))
	(simple-replace
	  (lambda (key keys value data)
	    (let* ((key-data (alist-ref key keys))
		   (start (car key-data))
		   (old-len (cadr key-data))
		   (delta (- new-len old-len)))
	      (make-ktext
		(realign-keys keys start delta)
		(string-replace data value start (+ start old-len)))))))
    (match `(,key ,ktext)
      [(() _) (error "Invalid key")]
      [((k) (keys data))
       (simple-replace k keys value data)]
      [((k . sk) (keys data))
       #f])))

(define (ktext-insert ktext key value #!key (before #f) (after #f) (index #f))
  (unless (or before after index)
    (error "You must specify one of the keyword arguments 'before', 'after', or 'index'))
  #f)

) ; END MODULE
