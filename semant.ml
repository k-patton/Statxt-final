(*
Darpan Choudhary || dc3292
Xiao Lim || xl2669
Zion Lin || zel2109
Katherine Patton || kyp2106 
*)

(* Semantic checking for the Statxt compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions, structs) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) = 
    let check_it checked binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check) 
       in to_check
  in 

  (**** Checking Global Variables ****)

  let globals' = check_binds "global" globals in



  (**** Checking Structs ****)

   let check_struct struc =
    (* Make sure no members are void or duplicates *)
    let members' = check_binds "member" struc.members in
    {
      ssname = struc.sname;
      smembers = members';
    } in

    (* Return a semantically-checked expression, i.e., with a type *)
    let structs' = List.map check_struct structs in

    let verify_struct sname element =
    	let s = try List.find (fun s -> s.sname = sname) structs
        with Not_found -> raise (Failure("Struct " ^ sname ^ " not found")) in
      let smembers = List.map (fun (ty, name) -> (ty, name)) s.members in
      try fst(List.find (fun f -> snd(f) = element) smembers) with
      Not_found -> raise (Failure("Field " ^ element ^ " not found in Struct" ^ sname))
    in
      
    let check_element_exist lhs rhs =
    	match lhs with
    		  Struct s -> verify_struct s rhs
    		| _ -> raise(Failure(string_of_typ lhs ^ " is not a struct"))
    in




  (**** Checking Functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =   
    let add_bind map (name, typ, forms) = StringMap.add name {
      typ = typ; fname = name; 
      formals = forms;
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Void, [(Int, "x")]);
			                         ("printb", Void, [(Bool, "x")]);
			                         ("printstr", Void, [(String, "x")]);
                               ("printchar", Void, [(Char, "x")]);
			                         ("printf", Void, [(Float, "x")]);
			                         ("printbig", Void, [(Int, "x")]);
                               ("strlen", Int, [(String, "x")]);
                               ("strcmp", Int, [(String, "x"); (String, "x")]);
                               ("strcpy", String, [(String, "x"); (String, "x")]);
                               ("strcat", String, [(String, "x"); (String, "x")]);
                               ("strget", Char, [(String, "x"); (Int, "y")]);
                               ("lower", String, [(String, "x")]);
                               ("open", String, [(String, "x"); (String, "x")]);
                               ("close", Int, [(String, "x")]);
                               ("read", Int, [(String, "x"); (Int, "x"); (Int, "x"); (String, "x")]);
                               ("write", Int, [(String, "x"); (Int, "x"); (Int, "x"); (String, "x")]);
                               ("calloc", String, [(Int, "x"); (Int, "y")]);
                               ("free", Int, [(String, "x")]);
                               ("putstr", Int, [(String, "x"); (String, "x")]);
                               ("atoi", Int, [(String, "x")]);
                               ("itoc", Char, [(Int, "x")]);
                               ("isletter", Bool, [(Char, "x")]);
                               ("strappend", Char, [(String, "x"); (Int, "x"); (Char, "x")]);
                               ("moveptr", String, [(String, "x"); (Int, "y")]);
                               ("substring", String, [(String, "x"); (Int, "y"); (Int, "z")])
                               ]     

                               (*StringMap.add "strlen" { 
    typ = Int; fname = "strlen"; formals = [(Int, "x")]; locals = []; body = [] }*)
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in
    let locals' = check_binds "local" func.locals in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals' @ formals' @ locals' )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Intlit  l -> (Int, SIntlit l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Strlit l   -> (String, SStrlit l)
      | Charlit l  -> (Char, SCharlit l)
      | Arraylit (eles, size) -> let e1 = List.hd eles in
      								let (ty, _) = expr e1 in
      								let _ = match ty with
	      								  Int -> ()
	      								| Bool -> ()
	      								| Float -> ()
	      								| String -> ()
	      								| Char -> ()
	      								| _ -> raise(Failure ("type not allowed in array"))
      								in let _ = List.iter (fun a -> if (fst(expr a) = ty) then () else raise(Failure("type mismatch"))) eles
      								in let seles = List.map expr eles
      								in (Array (ty, size(*(List.length eles)*)), SArraylit(seles, size)) (*not sure about the SNoexpr*)
      | Arraccess (exp0, exp) -> 
      	  let t = let _ = match (fst(expr exp)) with 
      	  	   Int -> Int
      	 	 | _ -> raise(Failure ("accessing array with non-integer type")) in
	      	  	match (fst(expr exp0)) with
	      	  	  Array(t, _) -> t (* if something breaks, this is the problem. returns tuple like string * a *)
	      	  	| _ -> raise(Failure ("trying to access a non-array type"))
	      in (t, SArraccess((expr exp0), (expr exp)))
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(e1, e2) as ex -> 
          let (lt, e1') = expr e1 in
          let (rt, e2') = expr e2 in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
            in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2')))
      | Sretrieve(str, element) ->  (check_element_exist (fst (expr str)) element, SSretrieve ((expr str), element))
      | IncDec(e, i) as ex -> let t = expr e in 
          (match i with
              | Inc | Dec -> (match (fst t) with 
                    Int -> ((fst t), SIncDec((Int, (snd t)), i))
                  |  _ -> raise (Failure ("illegal postfix operator " ^ string_of_incdec i ^ " used with a " ^
                                              string_of_typ (fst t) ^ " in " ^ string_of_expr ex))
              )
          )     
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    (*  | _ -> raise (Failure ("not implemented yet")) *)
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block (bl, sl) -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block (_, sl) :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(bl, (check_stmt_list sl))

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody = match check_stmt (Block ([], func.body)) with
	SBlock([], sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (globals', List.map check_function functions, structs')
