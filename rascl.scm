(module rascl-processor
        (set-system-config-path
         set-user-config-path
         load-config)

(define *system-config-path* (make-parameter #f))
(define *user-config-path* (make-parameter #f))

(define (set-system-config-path path)
  (*system-config-path* path))

(define (set-user-config-path path)
  (*user-config-path* path))

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
            (config-get arg data)
            (config-set arg args data)))))))
        

)
