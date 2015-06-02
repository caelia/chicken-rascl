(use utils extras comparse)
        (use data-structures)
        (use srfi-14)

;(define file-content
;(include "rascl-parser.scm"))

(include "rascl-parser.scm")
(import rascl-parser)


(let ((args (command-line-arguments)))
  (when (null? args)
    (error "Please specify an input file."))
  (with-input-from-file (car args)
    (lambda ()
      (let-values (((result remainder)
                    (parse dict-content (read-all (current-input-port)))))
        (pp result)))))
