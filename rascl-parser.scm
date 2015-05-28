;;; rascl-parser.scm -- A parser for the rascl egg.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module rascl-parser
        *
        (import scheme chicken)
        (import data-structures)
        (use utils)
        (use comparse)
        (use srfi-14)

(define whitespace
  (one-or-more (in char-set:whitespace)))

(define reserved
  (let ((cs:reserved (string->char-set "#:,{}[]\n")))
    (in cs:reserved)))

(define newline~
  (is #\newline))

(define eol
  (any-of newline~ end-of-input))

(define comment
  (preceded-by (is #\#)
               (maybe whitespace)
               (as-string (zero-or-more (none-of* newline~ item)))))

(define line-end
  (sequence (maybe comment) eol))

(define separator
  (sequence (maybe whitespace) (any-of (is #\,) line-end) (maybe whitespace)))

(define escape-sequence
  (let ((cs:esc (list->char-set '(#\alarm #\vtab #\page #\newline #\tab #\backspace))))
    (preceded-by (is #\\) (as-string (in cs:esc)))))

(define qstring-content
  (zero-or-more (any-of escape-sequence
                        (none-of* (is #\") item))))

(define qstring
  (enclosed-by (is #\") (as-string qstring-content) (is #\")))

(define ustring
  (followed-by (as-string (one-or-more (none-of* reserved line-end item)))
               (any-of reserved line-end)))

(define string~
  (any-of qstring ustring))

(define hex-int
  (bind (preceded-by (char-seq "0x") (as-string (one-or-more (in char-set:hex-digit))))
        (lambda (s) (result (conc "#x" s))))) 

(define dec-int
  (as-string (sequence (maybe (is #\-)) (one-or-more (in char-set:digit)))))

(define oct-int
  (let ((cs:oct-digit (string->char-set "01234567")))
    (bind (preceded-by (char-seq "0o") (as-string (one-or-more (in cs:oct-digit))))
          (lambda (s) (result (conc "#o" s))))))

(define integer
  (bind (any-of hex-int dec-int oct-int)
        (lambda (s) (result (string->number s)))))

(define float
  (sequence (maybe (is #\-))
            (any-of (sequence (one-or-more (in char-set:digit)) (is #\.)
                              (zero-or-more (in char-set:digit)))
                    (sequence (zero-or-more (in char-set:digit)) (is #\.)
                              (one-or-more (in char-set:digit))))))

(define number
  (any-of integer float))

(define boolean
  (let ((cs:t (string->char-set "Tt"))
        (cs:r (string->char-set "Rr"))
        (cs:u (string->char-set "Uu"))
        (cs:e (string->char-set "Ee"))
        (cs:f (string->char-set "Ff"))
        (cs:a (string->char-set "Aa"))
        (cs:l (string->char-set "Ll"))
        (cs:s (string->char-set "Ss")))
    (bind (as-string
            (any-of (sequence (in cs:t) (in cs:r) (in cs:u) (in cs:e))
                    (sequence (in cs:f) (in cs:a) (in cs:l) (in cs:s) (in cs:e))))
          (lambda (b)
            (result
              (cond
                ((string-ci=? b "true") #t)
                ((string-ci=? b "false") #f)))))))


(define int-list-content
  (sequence integer (zero-or-more (preceded-by separator integer))))

(define float-list-content
  (sequence float (zero-or-more (preceded-by separator float)))) 

(define number-list-content
  (sequence number (zero-or-more (preceded-by separator number))))

(define boolean-list-content
  (sequence boolean (zero-or-more (preceded-by separator boolean))))

(define string-list-content
  (sequence string~ (zero-or-more (preceded-by separator string~))))

(define list-content
  (bind (any-of int-list-content
                float-list-content
                number-list-content
                boolean-list-content
                string-list-content)
        (lambda (lst) (result (flatten lst)))))

(define list~
  (enclosed-by (is #\[)
               (any-of list-content (maybe whitespace))
               (is #\])))

(define dict-key string~)

; MRP stands for "mutually recursive parser"
(define-syntax mrp
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

(define dict-value
  (mrp
    (any-of integer float number boolean string~ list~ dict)))

(define dict-entry
  (sequence dict-key
            (maybe whitespace)
            (is #\:)
            (maybe whitespace)
            dict-value))

(define dict-content
  (sequence (maybe whitespace)
            (maybe (sequence dict-entry
                             (zero-or-more (sequence separator dict-entry))))
            (maybe whitespace)))

(define dict
  (enclosed-by (is #\{) dict-content (is #\})))

(define (parse-rascl-file filename)
  (with-input-from-file filename
    (lambda ()
      (parse dict-content (read-all (current-input-port))))))

) ; END MODULE

#|
{
  open RasclParser

  exception LexerError of string

  let sym_or_recurse symopt act =
    match symopt with
    | Some s -> s
    | None -> act

  let char_for_backslash ch =
    match ch with
    | 'a' -> '\007'
    | 'v' -> '\011'
    | 'f' -> '\012'
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c   -> c

  let string_buff = Buffer.create 256
  let reset_string_buffer () = Buffer.clear string_buff  
  let store_string_char c = Buffer.add_char string_buff c
  let store_string s = Buffer.add_string string_buff s
  let get_stored_string () = Buffer.contents string_buff    
}

bs_escapes = [ '\032' - '\255' ]
iws = [ ' ' '\t' ]*
btrue = [ 'T' 't' ] [ 'R' 'r' ] [ 'U' 'u' ] [ 'E' 'e' ]
bfalse = [ 'F' 'f' ] [ 'A' 'a' ] [ 'L' 'l' ] [ 'S' 's' ] [ 'E' 'e' ]
nfloat = [ '0'-'9' ]+ '.' [ '0'-'9' ]* | '.' [ '0'-'9' ]+
nhex = '0' 'x' [ '0'-'9' 'A'-'F' 'a'-'f' ]+
noct = '0' 'o' [ '0'-'7' ]+
ndec = [ '0'-'9' ]+
symbol =
    [^ '\000' - '\032' '0'-'9' '"' '#' ':' ',' '{' '}' '[' ']' '\\' ]
    [^ '\000' - '\032' '"' '#' ':' ',' '{' '}' '[' ']' '\\' ]*


rule dict = parse
    iws                    { dict lexbuf }
  | '#' [^ '\n']* '\n'     { SEP }
  | '"'                    { reset_string_buffer ();
                             inquotes lexbuf;
                             let s = get_stored_string () in
                             EXPR s }  
  | ':'                    { ASSN }
  | '{'                    { DS }
  | '}'                    { DE }
  | '['                    { LS }
  | ']'                    { LE }
  | ',' | '\n'             { SEP }
  | btrue                  { EXPR "true" } 
  | bfalse                 { EXPR "false" }
  | nfloat                 { EXPR (Lexing.lexeme lexbuf) }
  | nhex | noct | ndec     { EXPR (Lexing.lexeme lexbuf) }
  | symbol                 { SYMBOL (Lexing.lexeme lexbuf) }
  | eof                    { EOF }
and inquotes = parse
  | [ '"' ]    { () }
  | '\\' iws '\n' iws
      { store_string_char ' '; inquotes lexbuf }
  | '\\' (bs_escapes as c)  
      { store_string_char (char_for_backslash c); inquotes lexbuf }
  | '\n'       { raise ( LexerError "unterminated string" ) }
  | eof        { raise ( LexerError "unterminated string" ) }
  | _ as c     { store_string_char c; inquotes lexbuf }
|#

