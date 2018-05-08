(*
Darpan Choudhary || dc3292
Xiao Lim || xl2669
Zion Lin || zel2109
Katherine Patton || kyp2106 
*)

(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type incdec = 
  | Dec
  | Inc 

type uop = Neg | Not

type typ = Int | Bool | Float | Void | String | Char | Struct of string | Array of typ * int (*int is array size*)

type bind = typ * string

type expr =
    Intlit of int
  | Fliteral of string
  | Charlit of char
  | Strlit of string
  | BoolLit of bool
  | Id of string
  | Arraylit of expr list * int (* list of expressions and size of array *)
  | Arraccess of expr * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | IncDec of expr * incdec 
  | Assign of expr * expr
  | Sretrieve of expr * string
(*  | Arrassign of string * int * expr *)
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of bind list * stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals: bind list;
    body : stmt list;
  }

type struct_decl = {
  sname : string;
  members : bind list;
}

type program = bind list * func_decl list * struct_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_incdec = function 
    Inc -> "++"
  | Dec -> "--"

let rec string_of_expr = function
    Intlit(l) -> string_of_int l
  | Fliteral(l) -> l
  | Charlit(l) -> Char.escaped l
  | Strlit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Arraylit(exp, i) -> "[" ^ String.concat ", " (List.map string_of_expr exp) ^ "] (length: " ^ (string_of_int i) ^ ")"
  | Arraccess(s, exp) -> (string_of_expr s) ^ "[" ^ (string_of_expr exp) ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | IncDec(e, i) -> string_of_expr e ^ string_of_incdec i 
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Sretrieve(s, ele) -> string_of_expr s ^ "." ^ ele
 (* | Arrassign(a, inx, e) -> a ^ "[" ^ (string_of_int inx) ^ "] = " ^ string_of_expr e *)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(_, stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([], [])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"
  | Char -> "char"
  | Struct(id) -> "struct " ^ id
  | Array(s, i) -> "array length " ^ string_of_int i ^ " with type " ^ string_of_typ s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_sdecl sdecl =
  "struct" ^ " " ^ sdecl.sname ^ "{\n" ^ 
  String.concat "" (List.map string_of_vdecl sdecl.members) ^ "};\n"

let string_of_program (vars, funcs, structs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sdecl structs)
