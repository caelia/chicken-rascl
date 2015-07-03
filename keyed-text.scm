;;; keyed-text.scm -- Strings accessed by keys for RASCL.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module keyed-text
        *
        (import scheme chicken)
	(use matchable)

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
    [((k sk) (keys data))
     (let ((text-ref (alist-ref k keys))
  (let loop ((content (ktext-data ktext))
             (keys (ktext-keys ktext))
             (key2find key))
    (if (symbol? key2find)
      (get-sub key2find keys content)
      (let-values (((sub-cont sub-keys)
                    (get-sub (car key2find) keys content)))
        (loop sub-cont sub-keys (cdr key2find))))))

(define (ktext-insert! ktext key value)
  (let ((len (string-length value)))


) ; END MODULE
