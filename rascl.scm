;;; rascl.scm -- A RASCL implementation for Chicken Scheme.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module rascl
        (set-system-config-path
         set-user-config-path
         load-config)
        (import scheme chicken)
        (require-library rascl-parser)
        (reexport rascl-parser)

(define *system-config-path* (make-parameter #f))
(define *user-config-path* (make-parameter #f))

(define (set-system-config-path path)
  (*system-config-path* path))

(define (set-user-config-path path)
  (*user-config-path* path))

(define (load-config path)
  #f)

(define (merge-config conf1 conf2)
  #f)

(define (save-config conf)
  #f)

(define (check-config conf)
  #f)

(define (config-get conf key)
  #f)

(define (config-set! conf key value)
  #f)

(define (write-system-config data)
  #f)

(define (write-config data #!optional (write-all #f))
  #f)

(define (make-config)
  (let ((data '())
        (checksum #f))
    (lambda (arg . args)
      (case arg
        ((save)
         (save-config data))
        ((reload)
         (set! data
           (merge-config
             (load-config (*system-config-path*))
             (load-config (*user-config-path*)))))
        ((check)
         (check-config (*user-config-path*)))
        (else
          (if (null? args)
            (config-get data arg)
            (config-set! data arg args)))))))
        
)
