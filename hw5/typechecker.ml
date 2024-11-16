open Ast
open Astlib
open Tctxt



let and_l (list: bool list) = 
  List.fold_left (fun a b -> a && b) true list


let or_l (list: bool list) = 
  List.fold_left (fun a b -> a || b) false list


(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool , TBool -> true
  | TRef rty1, TRef rty2 -> subtype_ref c rty1 rty2
  | TNullRef rty1, TNullRef rty2 -> subtype_ref c rty1 rty2
  | _, _ -> false
  
and subtype_ret (c : Tctxt.t) (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =   
  match t1, t2 with
  | RetVoid, RetVoid -> true
  | RetVal ty1, RetVal ty2 -> subtype c ty1 ty2
  | _, _ -> false 
  


(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
      match t1, t2 with
      | RString, RString -> true
      | RArray sty1, RArray sty2 -> subtype c sty1 sty2 && subtype c sty2 sty1
      | RFun (ty_l1, ret_ty1),  RFun (ty_l2, ret_ty2) -> let check_args =  List.for_all2 (fun t1 t2 -> subtype c t1 t2) ty_l2 ty_l1 in 
            and_l check_args  &&  subtype_ret c ret_ty1 ret_ty2  
      | RStruct sid1, RStruct sid2 ->
        let fields_id2 = lookup_struct sid2 c in

        List.for_all (fun {fieldName = f_name; ftyp =  ty} ->

          match lookup_field_option sid1 f_name c with
          |None -> false
          |Some ty2 -> subtype c ty ty2 && subtype c ty2 ty

          ) fields_id2 
      | _, _ -> false

(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well-formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)

 
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | (TInt | TBool) -> ()
  | TRef rty | TNullRef rty  -> typecheck_rty l tc rty

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
  | RString -> ()
  | RArray ty -> typecheck_ty l tc ty
  | RStruct id -> 
    begin match lookup_struct_option id tc with
    |  None -> type_error l "struct not defined\n"
    |  Some field_l -> List.hd @@ List.map (fun {fieldName = _; ftyp = fty} -> typecheck_ty l tc fty) field_l
    end
  | RFun (ty_l, ret_ty) -> List.map (fun ty -> typecheck_ty l tc ty) ty_l; typecheck_ret_ty l tc ret_ty

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oad.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  let {elt= expr} = e in
   match expr with
    | CNull rty -> TNullRef rty
    | CBool _ -> TBool
    | CInt _ -> TInt
    | CStr _ -> TRef RString
    | Id id -> 
       begin match lookup_option id c with
        | None -> type_error e "id not present in type ctxt"
        | Some ty -> ty
        end

    | CArr (ty, expr_node_l) -> 
      let _ = typecheck_ty e c ty in
      let well_typed = expr_node_l |>  List.map (fun exp -> typecheck_exp c exp) 
                                |> List.for_all (fun ty_exp -> subtype c ty_exp ty) 
      in 
        begin match well_typed with
        | true -> TRef (RArray ty)
        | false -> type_error e "Array T[] initializers do not evaluate to sybtypes of T"
        end

    | NewArr (ty, len_exp, id, init_expr) ->
      let len_ty = typecheck_exp c len_exp in
      begin match len_ty with 
              |TInt -> 
                  let type_init = typecheck_exp c init_expr in
                  begin match subtype c type_init ty with
                  | true -> (TRef (RArray ty)) 
                  | false -> type_error e "Exp type of (T[] = NewArray) initializer expression not subtype of T "
                  end
              | _ -> type_error e "Length expression in an (T[] = NewArray) expression is not int"
      end

    | Length array_exp -> 
      begin match (typecheck_exp c array_exp) with
      | TRef RArray _ -> TInt
      | _ -> type_error e "method length applied to a non-array argument"
      end

    | CStruct (sid, field_l) -> 
      begin match lookup_struct_option sid c with
      | None -> type_error e "Struct not defined" (*maybe I should use typecheck_rty ???*)
      | Some sty_field_l -> 
          let comparator_tuples (id1, _) (id2, _) = (Hashtbl.hash id1) - (Hashtbl.hash id2) in
          let comparator_fields {fieldName = f1;_} {fieldName = f2;_} = (Hashtbl.hash f1) - (Hashtbl.hash f2) in 
          let sorted_inits = List.sort comparator_tuples field_l in 
          let sorted_fields = List.sort comparator_fields sty_field_l in
          let well_typed = List.for_all2 (fun (iname, exp1) {fieldName = fname; ftyp = fty} -> 
            String.equal iname fname &&  
            subtype c (typecheck_exp c exp1) fty) sorted_inits sorted_fields 
          in
          if well_typed then (TRef (RStruct sid)) else type_error e "Struct inititialization arguments not well-typed"
      end

   | Proj (str_exp, id) -> 
    let struct_fields =  
      begin match typecheck_exp c str_exp with 
      | TRef (RStruct id) -> lookup_struct id c
      | _ -> type_error e "Struct not defined"
      end
    in

    begin match List.find_opt (fun {fieldName = fname; ftyp = fty} -> String.equal fname id) struct_fields with
    | None -> type_error e "field not present in struct"
    | Some {fieldName = fname;ftyp =  fty} -> fty 
    end

    | Call (fun_exp, arg_exp_l) ->

      begin match (typecheck_exp c fun_exp) with
        | TRef (RFun (arg_l, ret_ty)) -> 

          let args_well_typed = List.for_all2 (fun arg_exp ty -> subtype c (typecheck_exp c arg_exp) ty) arg_exp_l arg_l 
          in
          begin match args_well_typed with 
          | true -> 
              begin match ret_ty with
              | RetVoid -> type_error e "void function in an expression"
              | RetVal t -> t 
              end
          | false -> type_error e "arguments passed to function are ill-typed"
          end

        | _ -> type_error e "function expression does not evaluate to type a function pointer"
      end

    | Bop (binop, exp1, exp2) -> 

      let op1_ty, op2_ty = (typecheck_exp c exp1), (typecheck_exp c exp2) in
      let mutual_subtypes = subtype c op1_ty op2_ty && subtype c op2_ty op1_ty in

      begin match binop with  
      | Eq | Neq ->
          begin match mutual_subtypes with
          |true -> TBool
          |false -> type_error e "comparison of incompatible types" 
          end
      |_ -> 
        let (ty1, ty2, ret_ty) = typ_of_binop binop in
        begin match op1_ty, op2_ty with
        | ty1, ty2 -> ret_ty
        | _, _ -> type_error e "Operands of binary op expr. are ill-typed" 
        end
      end 
      
    | Uop (uop, exp1) -> 
      let op1_ty = (typecheck_exp c exp1) in
      let (ty1, ret_ty) = typ_of_unop uop in
      begin match op1_ty with
      | ty1 -> ret_ty
      | _ -> type_error e "Operand of unary op expr. is ill-typed"
      end



(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statement typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement
     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, both branches must definitely return

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entier conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  failwith "todo: implement typecheck_stmt"


(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)

let separate_block l = 
  let rev_list = List.rev l in
  (rev_list |> List.tl |> List.rev, List.hd rev_list)  

let typecheck_block (c: Tctxt.t) (b: Ast.block) (l : 'a Ast.node) (ret_ty: ret_ty): bool =
  let (body, exit) = separate_block b
  in 
  let new_ctxt = List.fold_left (fun ctxt s -> 
    let new_c, def_returns = typecheck_stmt ctxt s ret_ty in
    if def_returns then type_error l "only the last statement of a block can defenitely return"
    else new_c
    ) c  body 
  in 
  let _, def_returns = typecheck_stmt new_ctxt exit ret_ty in
  if def_returns then ()
  else type_error l "last statement of block doesn't return" 
  

let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
    let tc_local = List.fold_left (fun ctxt (ty, id) -> Tctxt.add_local c id ty) tc f.args in
    let _ = typecheck_block tc_local f.body l f.frtyp in (*check body using statement*)
    let _ = List.iter (fun ty, _ -> typecheck_ty l tc ty) f.args in   (*check args: H |- t1, H |- t2 ...*)
    () (*both assumptions combinde to prove that H, G |- rty f(t1 t2 etc) block *)



(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  let f tc decl = 
    match decl with
    |  Gtdecl {elt = (id,bnd)} -> 
      begin match (lookup_struct_option id tc) with
      | None -> Tctxt.add_struct tc id bnd 
      | _ -> failwith "duplicated struct defs"
      end
    | _ -> tc 
  in
  List.fold_left f Tctxt.empty p 

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let f c decl = 
    match decl with
     Gfdecl{ elt ={frtyp = f_ret_ty; fname = id; args = ty_id_l}} -> 
      begin match lookup_global_option id c with
      | None -> 
        let arg_tys, _ = List.split ty_id_l in
        let bnd = TRef( RFun (arg_tys, f_ret_ty)) in
        Tctxt.add_global c id bnd 
      | _ -> failwith "redeclared function"
      end
    | _ -> c 
  in
  List.fold_left f tc p
  
let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  
  let f c decl = 
    match decl with
     | Gvdecl {elt = {name =id; init = init_exp}} -> (*check if it mentions other gdecls?*)
      begin match lookup_global_option id c with
      | None -> 
          let ty = typecheck_exp tc init_exp in
          Tctxt.add_global c id ty 
      | _ -> failwith "redeclared global var" 
      end 
    | _ -> c 
  in
  let new_entries = List.fold_left f Tctxt.empty p in
  let result = {tc with globals =  new_entries.globals @ tc.globals} in 
  result 



(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
