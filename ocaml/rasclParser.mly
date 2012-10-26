/* rasclParser.mly -- Copyright © 2006 by Matthew C. Gushee 
   This program is free software, released under the terms of the BSD
   license. See the file LICENSE for details. */

%{
  open RasclDict
  (* sdata_join is used to concatenate values consisting of
   * multiple symbols, but I don't think that will be supported. *)
  let sdata_join s1 (d2:data) =
    match d2 with
    | S s2 -> S (s1 ^ " " ^ s2)
    | _ -> failwith "Input to sdata_join must use S constructor"

  
  let add_item k v d =
    set k v d; d

%}

%token DS DE LS LE ASSN SEP EOF
%token <string> EXPR
%token <string> SYMBOL

%start config_dict
%type <t> config_dict


%%
config_dict :
    SYMBOL ASSN config_value SEP config_dict   { add_item $1 $3 $5 } 
  | SYMBOL ASSN config_value SEP EOF           { add_item $1 $3 (create ()) } 
  | SYMBOL ASSN config_value EOF               { add_item $1 $3 (create ()) } 
  | SYMBOL ASSN config_value SEP               { add_item $1 $3 (create ()) } 
  | SYMBOL ASSN config_value                   { add_item $1 $3 (create ()) } 
;
config_value :
    EXPR                                       { S $1 }
  | SYMBOL                                     { S $1 }
  | DS SEP config_dict DE                      { D $3 }
  | LS SEP config_list LE                      { L $3 }
  | DS config_dict DE                          { D $2 }
  | LS config_list LE                          { L $2 }
;
config_list :
    EXPR SEP config_list                       { $1::$3 }
  | SYMBOL SEP config_list                     { $1::$3 }
  | EXPR SEP                                   { [$1] }
  | SYMBOL SEP                                 { [$1] }
  | EXPR                                       { [$1] }
  | SYMBOL                                     { [$1] }
;

%%
