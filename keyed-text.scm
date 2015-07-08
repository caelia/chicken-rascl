;;; keyed-text.scm -- Strings accessed by keys for RASCL.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module keyed-text
        *
        (import scheme chicken)
  (use matchable)
  (use srfi-13)

(define-record-type keydata
  (make-keydata% start len subkeys meta)
  keydata?
  (start keydata-start keydata-start-set!)
  (len keydata-len keydata-len-set!)
  (subkeys keydata-subkeys keydata-subkeys-set!)
  (meta keydata-meta keydata-meta-set!))

(define (make-keydata start len #!key (subkeys '()) (meta '()))
  (make-keydata% start len subkeys meta))

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

(define (ktext-replace% key keys value text)
  (let* ((new-len
           (string-length value))
         (simple-replace
          (lambda (key keys value text)
            (let* ((key-data (alist-ref key keys))
                   (start (car key-data))
                   (old-len (cadr key-data))
                   (delta (- new-len old-len)))
              (values
                (realign-keys keys start delta)
                (string-replace text value start (+ start old-len)))))))
  (match key
    [() (error "Invalid key")]
    [(k) (simple-replace k keys value text)]
    [(k . sk) 
     (let-values (((subkeys subtext) (get-sub key keys text)))
       (ktext-replace% sk subkeys value subtext))]
    [k (simple-replace k keys value text)])))

(define (ktext-replace ktext key value)
  (match ktext
    [(keys data)
     (let-values (((newkeys newtext) (ktext-replace% key keys value data)))
       `(,newkeys ,newtext))]))

;  (let ((new-len
;          (string-length value))
;        (replace-parts
;          (lambda (key keys value data)
;            (let* ((key-data (alist-ref key keys))
;                   (start (car key-data))
;                   (old-len (cadr key-data))
;                   (delta (- new-len old-len)))
;              (values
;                (realign-keys keys start delta)
;                (string-replace data value start (+ start old-len))))))
;        (simple-replace
;          (lambda (key keys value data)
;            (let-values (((subkeys subtext) (replace-parts key keys value data)))
;              (make-ktext subkeys subtext))))
;        (rec-replace
;          (lambda (key keys value data)
;	    (let-values (((subkeys subtext) (replace-parts 
;    (match `(,key ,ktext)
;      [(() _) (error "Invalid key")]
;      [((k) (keys data))
;       (simple-replace k keys value data)]
;      [((k . sk) (keys data))
;       (let 
;      [((k) (keys data))
;       (simple-replace k keys value data)]

; POSITION must be one of '(before KEY), '(after KEY), 'at-start, or 'at-end 
(define (ktext-insert% key keys value text position)
  (match key
    [() (error "Invalid key")]
    [(k) 

(define (ktext-insert ktext key value position)
) ; END MODULE
