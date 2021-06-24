
{
open Flambda_parser

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
  | No_such_primitive of string
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid literal %s" s
  | No_such_primitive s -> Format.fprintf ppf "No such primitive %%%s" s

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let error ~lexbuf e = raise (Error (e, current_location lexbuf))

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "always", KWD_ALWAYS;
    "and", KWD_AND;
    "andwhere", KWD_ANDWHERE;
    "apply", KWD_APPLY;
    "Block", KWD_BLOCK;
    "ccall", KWD_CCALL;
    "closure", KWD_CLOSURE;
    "code", KWD_CODE;
    "cont", KWD_CONT;
    "default", KWD_DEFAULT;
    "deleted", KWD_DELETED;
    "direct", KWD_DIRECT;
    "done", KWD_DONE;
    "end", KWD_END;
    "error", KWD_ERROR;
    "exn", KWD_EXN;
    "fabricated", KWD_FABRICATED;
    "float", KWD_FLOAT;
    "halt_and_catch_fire", KWD_HCF;
    "hint", KWD_HINT;
    "imm", KWD_IMM;
    "immutable_unique", KWD_IMMUTABLE_UNIQUE;
    "in", KWD_IN;
    "inline", KWD_INLINE;
    "inlining_state", KWD_INLINING_STATE;
    "depth", KWD_INLINING_STATE_DEPTH;
    "int32", KWD_INT32;
    "int64", KWD_INT64;
    "let", KWD_LET;
    "mutable", KWD_MUTABLE;
    "nativeint", KWD_NATIVEINT;
    "never", KWD_NEVER;
    "newer_version_of", KWD_NEWER_VERSION_OF;
    "noalloc", KWD_NOALLOC;
    "rec", KWD_REC;
    "rec_info", KWD_REC_INFO;
    "set_of_closures", KWD_SET_OF_CLOSURES;
    "size", KWD_SIZE;
    "stub", KWD_STUB;
    "switch", KWD_SWITCH;
    "tupled", KWD_TUPLED;
    "unit", KWD_UNIT;
    "unreachable", KWD_UNREACHABLE;
    "unroll", KWD_UNROLL;
    "unsigned", KWD_UNSIGNED;
    "val", KWD_VAL;
    "where", KWD_WHERE;
    "with", KWD_WITH;
]

let ident_or_keyword str =
  try Hashtbl.find keyword_table str
  with Not_found -> IDENT str

let is_keyword str =
  Hashtbl.mem keyword_table str

let prim_table =
  create_hashtable [
    "Block", PRIM_BLOCK;
    "block_load", PRIM_BLOCK_LOAD;
    "get_tag", PRIM_GET_TAG;
    "int_comp", PRIM_INT_COMP;
    "is_int", PRIM_IS_INT;
    "Opaque", PRIM_OPAQUE;
    "phys_eq", PRIM_PHYS_EQ;
    "phys_ne", PRIM_PHYS_NE;
    "project_var", PRIM_PROJECT_VAR;
    "select_closure", PRIM_SELECT_CLOSURE;
    "Tag_imm", PRIM_TAG_IMM;
    "untag_imm", PRIM_UNTAG_IMM;
]

let prim ~lexbuf str =
  try Hashtbl.find prim_table str
  with Not_found -> error ~lexbuf (No_such_primitive str)

}

let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let int_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | "\n"
      { Lexing.new_line lexbuf; token lexbuf }
  | blank +
      { token lexbuf }
  | "(*"
      { comment 1 lexbuf;
        token lexbuf }
  | "let"
      { KWD_LET }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "+"  { PLUS }
  | "*"  { STAR }
  | "-"  { MINUS }
  | "<"  { LESS }
  | ">"  { GREATER }
  | "<=" { LESSEQUAL }
  | ">=" { GREATEREQUAL }
  | "+." { PLUSDOT }
  | "-." { MINUSDOT }
  | "=." { EQUALDOT }
  | "!=." { NOTEQUALDOT }
  | "<." { LESSDOT }
  | "<=." { LESSEQUALDOT }
  | "->" { MINUSGREATER }
  | "@" { AT }
  | "|"  { PIPE }
  | "===>" { BIGARROW }
  | identstart identchar* as ident
         { ident_or_keyword ident }
  | '`' ([^ '`' '\n']* as ident) '`'
         { IDENT ident }
  | '$' ((identchar* as ident) | '`' ([^ '`' '\n']* as ident) '`')
         { SYMBOL ident }
  | '%' (identchar* as p)
         { prim ~lexbuf p }
  | (int_literal as lit) (int_modifier as modif)?
         { INT (lit, modif) }
  | float_literal | hex_float_literal as lit
         { FLOAT (lit |> Float.of_string) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as lit
         { error ~lexbuf (Invalid_literal lit) }
  | eof  { EOF }
  | _ as ch
         { error ~lexbuf (Illegal_character ch) }

and comment n = parse
  | "\n"
         { Lexing.new_line lexbuf; comment n lexbuf }
  | "*)"
         { if n = 1 then ()
           else comment (n-1) lexbuf }
  | "(*"
         { comment (n+1) lexbuf }
  | _
         { comment n lexbuf }
