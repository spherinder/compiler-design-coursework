open Ast
open Astlib
open Tctxt


let rec take (i : int) (l : 'a list) : 'a list =
  if i <= 0 then []
  else match l with
    | [] -> []
    | x::xs -> x::take (i-1) xs


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
  | TRef rty1, TNullRef rty2 -> subtype_ref c rty1 rty2
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
      | RFun (ty_l1, ret_ty1),  RFun (ty_l2, ret_ty2) -> let check_args =  begin try List.for_all2 (fun t1 t2 -> subtype c t1 t2) ty_l2 ty_l1 with Invalid_argument _ -> false end in 
             check_args  &&  subtype_ret c ret_ty1 ret_ty2  

      | RStruct sid1, RStruct sid2 ->

        let fields_id1 = lookup_struct sid1 c
        and fields_id2 = lookup_struct sid2 c in

        let n = List.length fields_id2 in
        let first_fields1 = take n fields_id1 in
         
        if (List.length fields_id2 = 0) then true
        else 

        begin try 

          List.for_all2 (
          fun {fieldName = fname1; ftyp = ty1} {fieldName = fname2; ftyp = ty2 } ->
            String.equal fname1 fname2 && ty1 = ty2
          ) first_fields1 fields_id2 

          with Invalid_argument _ -> false end 

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
  | TInt | TBool -> ()
  | TRef rty | TNullRef rty  -> typecheck_rty l tc rty

and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
  | RString -> ()
  | RArray ty -> typecheck_ty l tc ty
  | RStruct id -> 
    begin match lookup_struct_option id tc with
    |  None -> type_error l "struct not defined\n"
    |  Some _ -> ()
    end
  | RFun (ty_l, ret_ty) -> let _ =  List.map (fun ty -> typecheck_ty l tc ty) ty_l in typecheck_ret_ty l tc ret_ty

and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty

(*Good*)

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
    | CNull rty -> let _ = typecheck_rty e c rty in TNullRef rty
    | CBool _ -> TBool
    | CInt _ -> TInt
    | CStr _ -> TRef RString
    | Id id -> 
       begin match lookup_option id c with
        | None -> type_error e (Printf.sprintf "id %s not present in type ctxt" id)
        | Some ty -> ty
        end

    | CArr (ty, expr_node_l) -> 
      let _ = typecheck_ty e c ty in
      let well_typed = expr_node_l |>  List.map (fun exp -> typecheck_exp c exp) 
                                |> List.for_all (fun ty_exp -> subtype c ty_exp ty) 
      in 
        if well_typed 
        then  TRef (RArray ty)
        else type_error e "Array T[] initializers do not evaluate to sybtypes of T"

    | NewArr (ty, len_exp, id, init_expr) ->
      let _ = typecheck_ty e c ty in
      let len_ty = typecheck_exp c len_exp
      and id_in_L = lookup_local_option id c in

      begin match len_ty, id_in_L with 
              |TInt, None -> 
                  let new_c = Tctxt.add_local c id TInt in
                  let type_init = typecheck_exp new_c init_expr in
                  if subtype c type_init ty 
                    then (TRef (RArray ty))
                    else type_error e "Exp type of (T[] = NewArray) initializer expression not subtype of T "
                    
              | _, _ -> type_error e "Length expression in an (T[] = NewArray) expression is not int or id shadows local context"
      end

    | Length array_exp -> 
      begin match (typecheck_exp c array_exp) with
      | TRef RArray _ -> TInt
      | _ -> type_error e "Method length applied to a non-array argument"
      end

    | CStruct (sid, field_l) -> 
      begin match lookup_struct_option sid c with
      | None -> type_error e "Struct not defined" 
      | Some sty_field_l -> 
          let comparator_tuples (id1, _) (id2, _) = (Hashtbl.hash id1) - (Hashtbl.hash id2) in
          let comparator_fields {fieldName = f1;_} {fieldName = f2;_} = (Hashtbl.hash f1) - (Hashtbl.hash f2) in 
          let sorted_inits = List.sort comparator_tuples field_l in 
          let sorted_fields = List.sort comparator_fields sty_field_l in
          let well_typed = begin try List.for_all2 (fun (iname, exp1) {fieldName = fname; ftyp = fty} -> 
            String.equal iname fname &&  
            subtype c (typecheck_exp c exp1) fty) sorted_inits sorted_fields with Invalid_argument _ -> false end
          in
          if well_typed then (TRef (RStruct sid)) else type_error e "Struct inititialization arguments ill-typed"
      end

   | Proj (str_exp, id) -> 
    let struct_fields =  
      begin match typecheck_exp c str_exp with 
      | TRef (RStruct id) -> begin try lookup_struct id c with Not_found -> type_error e "Access to a field in an undefined stuct" end
      | _ -> type_error e "Field access in a non-struct type"
      end
    in

    begin match List.find_opt (fun {fieldName = fname; ftyp = fty} -> String.equal fname id) struct_fields with
    | Some {fieldName = fname;ftyp =  fty} -> fty 
    | None -> type_error e "field not present in struct"
    end

  | Call (fun_exp, arg_exp_l) ->

      begin match (typecheck_exp c fun_exp) with
        | TRef (RFun (arg_l, ret_ty)) -> 

          let args_well_typed = begin try List.for_all2 (fun arg_exp ty -> subtype c (typecheck_exp c arg_exp) ty) arg_exp_l arg_l 
                                with Invalid_argument _ -> false end
          in

          if args_well_typed 
          then begin match ret_ty with
            | RetVoid -> type_error e "Void function in an expression"
            | RetVal t -> t 
            end
          else type_error e "Arguments passed to function are ill-typed"

        | _ -> type_error e "Function expression does not evaluate to type a function pointer"
      end

  | Bop (binop, exp1, exp2) -> 

      let op1_ty = (typecheck_exp c exp1)  
      and op2_ty = (typecheck_exp c exp2) in 

      begin match binop with  
      | Eq | Neq ->
          if (op1_ty = op2_ty) 
          then TBool
          else type_error e "comparison of incompatible types" 
      |_ -> 
        let (ty1, ty2, ret_ty) = typ_of_binop binop in
          if  ty1 = op1_ty &&  ty2 = op2_ty 
          then ret_ty
          else type_error e "Operands of binary op expr. are ill-typed" 
      end 
      
  | Uop (uop, exp1) -> 
      
      let op1_ty = (typecheck_exp c exp1) in
      let (ty1, ret_ty) = typ_of_unop uop in

      if op1_ty = ty1 
      then ret_ty
      else type_error e "Operand of unary op expr. is ill-typed"

  | Index (exp1, exp_inx) ->

    let inx_ty = typecheck_exp c exp_inx in 
    let exp_ty = typecheck_exp c exp1 in
      begin match exp_ty, inx_ty with
      | TRef(RArray ty), TInt-> ty 
      | _ -> type_error e "Index in index expr not of type int"
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

   - You will probably find it convenient to add a helper function that impled
*)



let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  let {elt = stmt} = s in

  match stmt with

  | Decl (id, exp) -> 
    begin match (lookup_local_option id tc) with
    | Some _ -> type_error s "Variable redeclared"
    | None -> 
      let exp_ty = typecheck_exp tc exp in
      let new_tc = Tctxt.add_local tc id exp_ty in 
      (new_tc, false)
    end

  | Assn (exp_lhs, exp_rhs) -> 
    
    let ty_lhs = typecheck_exp tc exp_lhs 
    and ty_rhs = typecheck_exp tc exp_rhs in
    
    begin match exp_lhs, ty_lhs with 

    | ({elt = Id id}), (TRef(RFun _ )) -> 
      begin match (lookup_global_option id tc) with
        |Some _ -> type_error s "Tried to assign with a function id on rhs" 
        |None -> let subtype_check = subtype tc ty_rhs ty_lhs in
                if subtype_check 
                then (tc, false)
                else type_error s "Type of rhs not subtype of lhs"  
        end
    | _, _ -> let subtype_check = subtype tc ty_rhs ty_lhs in
                if subtype_check 
                then (tc, false)
                else type_error s "Type of rhs not subtype of lhs"  

    end

  | Ret return_exp -> 
    begin match return_exp, to_ret with
    | None, RetVoid -> (tc, true) 
    | Some exp, RetVal ty -> 
      let ty_exp = typecheck_exp tc exp in 
        if subtype tc ty_exp ty
        then (tc, true)
        else type_error s "Return type incompatible with the type of function return" 
    | _, _ -> type_error s "Return type mismatch"  
    end

  | SCall (f_exp, arg_exp_l) -> 
      let f_ty = typecheck_exp tc f_exp in
      begin match f_ty with
      | TRef (RFun (f_arg_tys, RetVoid)) ->
        let well_typed =
                      begin try 
                        List.for_all2 (fun exp_arg ty_param -> 
                        let ty_arg = typecheck_exp tc exp_arg in subtype tc ty_arg ty_param ) 
                        arg_exp_l f_arg_tys         (*And arguments have the right types*)
                        with 
                        Invalid_argument _ -> false 
                      end

          in
          if well_typed 
          then (tc, false) 
          else type_error s "function arguments not well-typed" 

      | _ -> type_error s "function called but return value never used" 

      end
    
  | If (exp_condition, block1, block2) ->
    begin match typecheck_exp tc exp_condition with
    | TBool -> 
        let returns_block1 = typecheck_block tc block1 to_ret
        and returns_block2 = typecheck_block tc block2 to_ret in
        (tc, returns_block1 && returns_block2)
    | _ -> type_error s "condition expression doesn't return boolean"
    end

  | While (exp_condition, block) -> 
    begin match typecheck_exp tc exp_condition with
    | TBool -> 
      let _ = typecheck_block tc block to_ret in
      (tc, false)
    | _ -> type_error s "condition expression doesn't return boolean"
    end
  | For (vdecls, Some test_exp, Some inc_s, block) -> 
    let new_c = List.fold_left (fun c (id, decl_exp) -> 
      Tctxt.add_local c id (typecheck_exp c decl_exp)) tc vdecls
    in
      begin match (typecheck_exp new_c test_exp) with
      | TBool -> 
        let _, inc_returns = typecheck_stmt new_c inc_s to_ret in
        let _ = typecheck_block new_c block to_ret in
        if inc_returns
        then type_error s "increment of loop stmt returns"
        else (tc, false) 

      | _ -> type_error s "for-loop test expression doesn't return boolean"
      end
  | Cast (rty, id, exp, block1, block2) -> 
    let ty_rhs = typecheck_exp tc exp in
    
    begin match ty_rhs with
    | TNullRef(ref_rhs) ->
      if (subtype tc (TRef ref_rhs) (TRef rty) ) 
        then 
          let new_c = Tctxt.add_local tc id (TRef rty) in
          let ret1 = typecheck_block new_c block1 to_ret in
          let ret2 = typecheck_block tc block2 to_ret in
          (tc, ret1 && ret2)
        else type_error s "Rhs not a subtype of lhs in cast expression."
    | _ -> type_error s "rhs doesn't evaluate to a TNullRef type"
    end
  | _ ->  type_error s "Unmatched case in check_stmt"


and separate_block list = 
  let rev_list = List.rev list in
  (rev_list |> List.tl |> List.rev, List.hd rev_list)  

and typecheck_block (c: Tctxt.t) (b: Ast.block) (ret_ty: ret_ty): bool  =
  if List.length b = 0 then false else 

  let (body, exit) = separate_block b
  in 
  let new_ctxt = List.fold_left (fun ctxt s -> 
    let new_c, def_returns = typecheck_stmt ctxt s ret_ty in
    if def_returns then type_error s "only the last statement of a block can defenitely return"
    else new_c
    ) c  body 
  in 
  let (_, block_def_returns) = typecheck_stmt new_ctxt exit ret_ty in
  block_def_returns


  


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

  

let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =

    let _ = List.iter (fun (ty, _) -> typecheck_ty l tc ty) f.args in   (*check args: H |- t1, H |- t2 ...*)
    let tc_local = List.fold_left (fun ctxt (ty, id) -> Tctxt.add_local ctxt id ty) tc f.args in
    let return = typecheck_block tc_local f.body f.frtyp in (*check body using statement*) 
    if return then () else type_error l "Function does not return"
    (*both assumptions combinde to prove that H, G |- rty f(t1 t2 etc) block *)


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
let g0 = List.fold_left (fun c (str, (arg_tys, ret_ty) ) -> Tctxt.add_global c str (TRef (RFun (arg_tys, ret_ty))) ) Tctxt.empty builtins

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  let f tc decl = 
    match decl with
    |  Gtdecl ({elt = (id,bnd)} as node) -> 
      begin match (lookup_struct_option id tc) with
      | None -> Tctxt.add_struct tc id bnd 
      | _ -> type_error node "struct redefined" 
      end
    | _ -> tc
    in
  List.fold_left f g0 p 

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let f c decl = 
    match decl with
     Gfdecl ({ elt ={frtyp = f_ret_ty; fname = id; args = ty_id_l}} as node)-> 
      begin match lookup_global_option id c with
      | None -> 
        let arg_tys, _ = List.split ty_id_l in
        let bnd = TRef( RFun (arg_tys, f_ret_ty)) in
        Tctxt.add_global c id bnd 
      | _ -> type_error node "function redefined" 
      end
    | _ -> c 
  in
  List.fold_left f tc p
  
let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let f c decl = 
    match decl with
     | Gvdecl ({elt = {name =id; init = init_exp}} as node)-> 
      begin match lookup_global_option id c with
      | None -> 
          let ty = typecheck_exp tc init_exp in
          Tctxt.add_global c id ty 
      | _ -> type_error node "global variable redefined" 
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
