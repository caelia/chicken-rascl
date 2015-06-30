;;; keyed-text.scm -- Strings accessed by keys for RASCL.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module keyed-text
        *
        (import scheme chicken)

(define-record-type ktext
  (make-ktext% keys data)
  ktext?
  (keys ktext-keys ktext-keys-set!)
  (data ktext-data ktext-data-set!))

(define (make-ktext #!key (keys '()) (data #f))
  (make-ktext% keys data))

(define (ktext-size ktext)
  (let ((data (ktext-data ktext)))
    (if (ktext? data)
      (ktext-size data)
      (string-length data))))

(define (get-sub key keys text)
  (let* ((key-data (alist-ref key keys))
         (idx (car key-data))
         (len (cadr key-data))
         (subkeys (cddr key-data)))
    (values
      (substring text idx (+ idx len))
      subkeys)))

(define (ktext-ref ktext key)
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
