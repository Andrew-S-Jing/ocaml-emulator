(* 
                         CS 51 Final Project
                      MiniML -- Lexical Analyzer

 *)

{
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 8 [
                       ("if", IF);
                       ("in", IN);
                       ("then", THEN);
                       ("else", ELSE);
                       ("let", LET);
                       ("raise", RAISE);
                       ("rec", REC);
                       ("true", TRUE);
                       ("false", FALSE);
                       ("fun", FUNCTION);
                       ("function", FUNCTION);
                       ("head", HEAD);
                       ("tail", TAIL);
                       ("ref", REF);
                     ]
                     
  let sym_table = 
    create_hashtable 8 [
                       ("=", EQUALS);
                       ("<", LESSTHAN);
                       (".", DOT);
                       ("->", DOT);
                       (";;", EOF);
                       ("~-.", FNEG);
                       ("~-", NEG);
                       ("+", PLUS);
                       ("-", MINUS);
                       ("*", TIMES);
                       ("/", DIVIDE);
                       ("(", OPEN);
                       (")", CLOSE);
                       ("+.", FPLUS);
                       ("-.", FMINUS);
                       ("*.", FTIMES);
                       ("/.", FDIVIDE);
                       ("^", CONCAT);
                       ("::", CONS);
                       ("[", LOPEN);
                       ("]", LCLOSE);
                       (";", SCOLON);
                       ("!", DEREF);
                       (":=", ASSIGN);
                     ]
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let sym =
  ['(' ')' '[' ']']
  | (['$' '&' '*' '+' '-' '/' '=' '<' '>' '^' '.' '~' ';' '!' '?' '%' ':' '#']+)
let char = ['a'-'z' 'A'-'Z' '0'-'9' ' ']

rule token = parse
  | "()"
        { UNIT }
  | "[]"
        { LEMPTY }
  | digit+ '.' digit+? as ifloat
        { let f = float_of_string ifloat in
          FLOAT f
        }
  | digit+ as inum
        { let num = int_of_string inum in
          INT num
        }
  | ''' (char as c) '''
        { CHAR c }
  | "\"\""
        { NOSTRING }
  | '"' (char+ as s) '"'
        { STRING s }
  | id as word
        { try
            let token = Hashtbl.find keyword_table word in
            token 
          with Not_found ->
            ID word
        }
  | sym as symbol
        { try
            let token = Hashtbl.find sym_table symbol in
            token
          with Not_found ->
            printf "Ignoring unrecognized token: %s\n" symbol;
            token lexbuf
        }
  | '{' [^ '\n']* '}'   { token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']     { token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn & skip unrecognized chars *)
        { printf "Ignoring unrecognized character: %c\n" c;
          token lexbuf
        }
  | eof
        { raise End_of_file }
