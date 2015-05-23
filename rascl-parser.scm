;;; rascl-parser.scm -- A parser for the rascl egg.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

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

(module rascl-parser
        *
        (import scheme chicken)
        (use comparse)


) ; END MODULE
