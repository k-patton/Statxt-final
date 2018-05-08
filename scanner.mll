(* 
Darpan Choudhary || dc3292
Xiao Lim || xl2669
Zion Lin || zel2109
Katherine Patton || kyp2106 
*)

(* Ocamllex scanner for Statxt *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let chars = '''([' '-'!' '#'-'[' ']'-'~' ])'''
let strings = '"'([' '-'!' '#'-'[' ']'-'~' ]*)'"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"	   { sameline_comment lexbuf}
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| '|'	   { PIPE }
| '.'      { DOT }
| "++"	   { INCREMENT}
| "--"	   { DECREMENT}
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "string" { STRING }
| "char"   { CHAR }
| "struct" { STRUCT }
| "["	   { LSQUARE }
| "]"	   { RSQUARE }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { INTLIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| chars as lxm { CHARLIT(String.get lxm 1) }
| strings as lxm { STRLIT(String.sub lxm 1 (String.length lxm - 2)) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and sameline_comment = parse
  '\n' { token lexbuf }
| _	   { sameline_comment lexbuf }
