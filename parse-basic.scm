(use utils extras comparse)
        (use data-structures)
        (use srfi-14)

;(define file-content
;(include "rascl-parser.scm"))

(include "rascl-parser.scm")
(import rascl-parser)

; (define file-content dict-content)

(with-input-from-file "examples/basic.rsc"
  (lambda ()
    (let-values (((result remainder)
                  (parse dict-content (read-all (current-input-port)))))
      (pp result))))
; (let ((p (open-input-file "examples/basic.rsc")))
;   (display "file content: ") (pp file-content)
;   (display "parse: ") (pp parse)
;   (pp (parse file-content (read-string #f p)))
;   (close-input-port p))

; (let-values (((result remainder)
;               (parse-rascl-file "examples/basic.rsc")))
;   (if result
;     (pp result)
;     (print "NO result")))
