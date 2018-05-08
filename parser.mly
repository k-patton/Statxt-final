/* 
Darpan Choudhary || dc3292
Xiao Lim || xl2669
Zion Lin || zel2109
Katherine Patton || kyp2106 
*/

/* Ocamlyacc parser for Statxt */

%{
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

%}

%token LBRACE RBRACE
%token SEMI LPAREN RPAREN COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN DECREMENT INCREMENT
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID
%token STRING CHAR STRUCT LSQUARE RSQUARE PIPE DOT
%token <char> CHARLIT
%token <int> INTLIT
%token <bool> BLIT
%token <string> ID FLIT STRLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left DECREMENT INCREMENT
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left LSQUARE RSQUARE
%left DOT
%right NOT NEG


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [], []) }
 | decls vdecl   { (($2 :: fst $1), snd $1, trd $1) }
 | decls fdecl   { (fst $1, ($2 :: snd $1), trd $1) }
 | decls sdecl   { (fst $1, snd $1, ($2 :: trd $1)) }

fdecl:
   special_type ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
     fname   = $2;
     formals = $4;
     locals  = List.rev (fst $7);
     body    = List.rev (snd $7) } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    special_type ID                   { [($1, $2)]     }
  | formal_list COMMA special_type ID { ($3, $4) :: $1 }

sdecl:
  STRUCT ID LBRACE vdecl_list RBRACE SEMI
    { { sname = $2;
  members = $4 } }

typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | VOID   { Void   }
  | STRING { String }
  | CHAR   { Char   }
  | STRUCT ID { Struct ($2) }


special_type:
    typ   { $1 }
  | special_type LSQUARE INTLIT RSQUARE { Array($1, $3) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    special_type ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */   { [], [], [] }
  | stmt_list stmt  { fst $1, ($2 :: (snd $1)), [] }
  | stmt_list vdecl { ($2 :: (fst $1)), snd $1, [] }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev (fst $2), List.rev (snd $2)) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([], [])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

array_list:
    expr { $1 :: [] }
  | array_list COMMA expr { $3 :: $1 }

expr:
    CHARLIT          { Charlit ($1)           }
  | INTLIT           { Intlit($1)             }
  | BLIT             { BoolLit($1)            }
  | FLIT             { Fliteral($1)           }  
  | STRLIT           { Strlit($1)             }
  | ID               { Id($1)                 }
  | expr INCREMENT   { IncDec($1, Inc)        }
  | expr DECREMENT   { IncDec($1, Dec)        }  
  | expr LSQUARE expr RSQUARE { Arraccess($1, $3) }
  | LSQUARE array_list RSQUARE { Arraylit(List.rev $2, List.length $2) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | expr ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | expr DOT ID 		 { Sretrieve ($1, $3)     }
 /* | ID LSQUARE expr RSQUARE ASSIGN expr { Arrassign($1, $3, $6) } */
  | LPAREN expr RPAREN { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                 { [$1]     }
  | args_list COMMA expr { $3 :: $1 }
