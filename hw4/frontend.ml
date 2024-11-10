open Ll
open Llutil
open Ast



(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)
let debug = ref false  
type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None -> 
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some term ->
              (gs, einsns, [], None, (l, {insns; term})::blks)
           end
        | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator" 
    | Some term -> 
       let insns = einsns @ insns in
       ({insns; term}, blks), gs
(*printing helpers ---------------------------------------------------------- *)

let string_of_elt (elt:elt) = 
  match elt with
  |L lbl -> lbl
  | E (uid, term)  -> Llutil.string_of_named_insn (uid, term)
  | I (uid, term)  -> Llutil.string_of_named_insn (uid, term)
  | G (gid, decl) -> gid ^ " " ^ Llutil.string_of_gdecl decl
  | T term -> Llutil.string_of_terminator term

let string_of_stream stream = String.concat "\n" @@ List.map (fun elt -> (string_of_elt elt)) stream
  


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None
  
end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) -> 
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* Compiler Invariants

   The LLVM IR type of a variable (whether global or local) that stores an Oat
   array value (or any other reference type, like "string") will always be a
   double pointer.  In general, any Oat variable of Oat-type t will be
   represented by an LLVM IR value of type Ptr (cmp_ty t).  So the Oat variable
   x : int will be represented by an LLVM IR value of type i64*, y : string will
   be represented by a value of type i8**, and arr : int[] will be represented
   by a value of type {i64, [0 x i64]}**.  Whether the LLVM IR type is a
   "single" or "double" pointer depends on whether t is a reference type.

   We can think of the compiler as paying careful attention to whether a piece
   of Oat syntax denotes the "value" of an expression or a pointer to the
   "storage space associated with it".  This is the distinction between an
   "expression" and the "left-hand-side" of an assignment statement.  Compiling
   an Oat variable identifier as an expression ("value") does the load, so
   cmp_exp called on an Oat variable of type t returns (code that) generates a
   LLVM IR value of type cmp_ty t.  Compiling an identifier as a left-hand-side
   does not do the load, so cmp_lhs called on an Oat variable of type t returns
   and operand of type (cmp_ty t)*.  Extending these invariants to account for
   array accesses: the assignment e1[e2] = e3; treats e1[e2] as a
   left-hand-side, so we compile it as follows: compile e1 as an expression to
   obtain an array value (which is of pointer of type {i64, [0 x s]}* ).
   compile e2 as an expression to obtain an operand of type i64, generate code
   that uses getelementptr to compute the offset from the array value, which is
   a pointer to the "storage space associated with e1[e2]".

   On the other hand, compiling e1[e2] as an expression (to obtain the value of
   the array), we can simply compile e1[e2] as a left-hand-side and then do the
   load.  So cmp_exp and cmp_lhs are mutually recursive.  [[Actually, as I am
   writing this, I think it could make sense to factor the Oat grammar in this
   way, which would make things clearer, I may do that for next time around.]]

 
   Consider globals7.oat

   /--------------- globals7.oat ------------------ 
   global arr = int[] null;

   int foo() { 
     var x = new int[3]; 
     arr = x; 
     x[2] = 3; 
     return arr[2]; 
   }
   /------------------------------------------------

   The translation (given by cmp_ty) of the type int[] is {i64, [0 x i64]}* so
   the corresponding LLVM IR declaration will look like:

   @arr = global { i64, [0 x i64] }* null

   This means that the type of the LLVM IR identifier @arr is {i64, [0 x i64]}**
   which is consistent with the type of a locally-declared array variable.

   The local variable x would be allocated and initialized by (something like)
   the following code snippet.  Here %_x7 is the LLVM IR uid containing the
   pointer to the "storage space" for the Oat variable x.

   %_x7 = alloca { i64, [0 x i64] }*                              ;; (1)
   %_raw_array5 = call i64*  @oat_alloc_array(i64 3)              ;; (2)
   %_array6 = bitcast i64* %_raw_array5 to { i64, [0 x i64] }*    ;; (3)
   store { i64, [0 x i64]}* %_array6, { i64, [0 x i64] }** %_x7   ;; (4)

   (1) note that alloca uses cmp_ty (int[]) to find the type, so %_x7 has 
       the same type as @arr 

   (2) @oat_alloc_array allocates len+1 i64's 

   (3) we have to bitcast the result of @oat_alloc_array so we can store it
        in %_x7 

   (4) stores the resulting array value (itself a pointer) into %_x7 

  The assignment arr = x; gets compiled to (something like):

  %_x8 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7     ;; (5)
  store {i64, [0 x i64] }* %_x8, { i64, [0 x i64] }** @arr       ;; (6)

  (5) load the array value (a pointer) that is stored in the address pointed 
      to by %_x7 

  (6) store the array value (a pointer) into @arr 

  The assignment x[2] = 3; gets compiled to (something like):

  %_x9 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7      ;; (7)
  %_index_ptr11 = getelementptr { i64, [0 x  i64] }, 
                  { i64, [0 x i64] }* %_x9, i32 0, i32 1, i32 2   ;; (8)
  store i64 3, i64* %_index_ptr11                                 ;; (9)

  (7) as above, load the array value that is stored %_x7 

  (8) calculate the offset from the array using GEP

  (9) store 3 into the array

  Finally, return arr[2]; gets compiled to (something like) the following.
  Note that the way arr is treated is identical to x.  (Once we set up the
  translation, there is no difference between Oat globals and locals, except
  how their storage space is initially allocated.)

  %_arr12 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** @arr    ;; (10)
  %_index_ptr14 = getelementptr { i64, [0 x i64] },                
                 { i64, [0 x i64] }* %_arr12, i32 0, i32 1, i32 2  ;; (11)
  %_index15 = load i64, i64* %_index_ptr14                         ;; (12)
  ret i64 %_index15

  (10) just like for %_x9, load the array value that is stored in @arr 

  (11)  calculate the array index offset

  (12) load the array value at the index 

*)

(* Global initialized arrays:

  There is another wrinkle: To compile global initialized arrays like in the
  globals4.oat, it is helpful to do a bitcast once at the global scope to
  convert the "precise type" required by the LLVM initializer to the actual
  translation type (which sets the array length to 0).  So for globals4.oat,
  the arr global would compile to (something like):

  @arr = global { i64, [0 x i64] }* bitcast 
           ({ i64, [4 x i64] }* @_global_arr5 to { i64, [0 x i64] }* ) 
  @_global_arr5 = global { i64, [4 x i64] } 
                  { i64 4, [4 x i64] [ i64 1, i64 2, i64 3, i64 4 ] }

*) 



(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the stack, in bytes.  
 Note that since structured values are manipulated by reference, all
 Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate a zero-initialized array of source type TRef (RArray t) of the
 given size. Note "size" is an operand whose value can be computed at
 runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
let ans_id, arr_id = gensym "array", gensym "raw_array" in
let ans_ty = cmp_ty @@ TRef (RArray t) in
let arr_ty = Ptr I64 in
ans_ty, Id ans_id, lift
  [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
  ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]

(* Compiles an expression exp in context c, outputting the Ll operand that will
 recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to make sure
     either that the resulting gid has type (Ptr I8), or, if the gid has type
     [n x i8] (where n is the length of the string), convert the gid to a 
     (Ptr I8), e.g., by using getelementptr.

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

*)

(* 
current invariants
- each case assumes that the (recursively-computed) operands have been passed through a pointer. 
Each case must load them before working with them 
- Accordingly, each case returns a pointer to the location where it has stored the result of the expression.
- Each case returns the type of the result STORED IN the location it points to

I made this choices because this gives us freedom - we can reuse cmp_exp for both side of the Assn
statement.
   
*)


let cmp_bop (bop: Ast.binop) (op1: Ll.operand) (op2: Ll.operand) (res_reg: string): Ll.ty * stream  = 
  match bop with
  |Ast.Add ->  (I64, [I(res_reg, Binop (Add, I64, op1, op2))])
  |Ast.Sub ->  (I64, [I(res_reg, Binop (Sub, I64, op1, op2))])
  |Ast.Mul->  (I64, [I(res_reg, Binop (Mul, I64, op1, op2))])
  |Ast.Shl -> (I64, [I(res_reg, Binop (Shl, I64, op1, op2))]) 
  |Ast.Shr -> (I64, [I(res_reg, Binop (Lshr, I64, op1, op2))]) 
  |Ast.Sar -> (I64, [I(res_reg, Binop (Ashr, I64, op1, op2))]) 
  |Ast.Lt -> (I1, [I(res_reg, Icmp(Slt, I64, op1, op2))])
  |Ast.Lte -> (I1, [I(res_reg, Icmp(Sle, I64, op1, op2))])
  |Ast.Gt -> (I1, [I(res_reg, Icmp(Sgt, I64, op1, op2))])
  |Ast.Gte -> (I1, [I(res_reg, Icmp(Sge, I64, op1, op2))])
  |Ast.Eq -> (I1, [I(res_reg, Icmp(Eq, I64, op1, op2))])
  |Ast.Neq -> (I1, [I(res_reg, Icmp(Ne, I64, op1, op2))])
  |Ast.Or -> (I1, [I(res_reg, Binop (Or, I1, op1, op2))])
  |Ast.And -> (I1, [I(res_reg, Binop(And, I1, op1, op2))])
  |Ast.IAnd -> (I64, [I(res_reg, Binop(And, I64, op1, op2))])
  |Ast.IOr -> (I64, [I(res_reg, Binop(Or, I64, op1, op2))])
  
let cmp_uop (uop: Ast.unop) (op1: Ll.operand) (res_reg:string) : Ll.ty * stream = 
  match uop with
  | Neg -> (I64, [I(res_reg, Binop(Sub, I64, Const 0L, op1))])
  | Lognot -> (I1, [I(res_reg, Binop(Xor, I1, Const 1L, op1))])
  | Bitnot -> (I64, [I(res_reg, Binop(Xor, I64, (1L |> Int64.neg |>  (fun i -> Const i)) , op1))])

let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
let {elt = expr} = exp 
and op1_reg = ref (gensym "op_a")
and op2_reg = ref (gensym "op_b")
and res_reg = ref (gensym "result_bop")
and ret_reg = ref (gensym "return")
and stack_reg = ref (gensym "stack_ref")
in 
match expr with
| CBool b -> I1, Id !ret_reg, 
  [
    E (!ret_reg, Alloca I1) ; 
    I (gensym "tmp", Store (I1, b |> Bool.to_int |> Int64.of_int |> (fun a -> Const a) , Id !ret_reg )) 
  ]
| CInt num -> I64, Id !ret_reg, 
  [
    E (!ret_reg, Alloca I64); 
    I (gensym "tmp", Store (I64, Const num , Id !ret_reg))
  ] 
| CNull rty -> Ptr(cmp_rty rty), Id !ret_reg, 
    [
      E (!ret_reg, Alloca (Ptr (cmp_rty rty))); 
        I (gensym "tmp", Store (Ptr (cmp_rty rty) , Null, Id !ret_reg))
      ] 
  | CStr str -> let global_id =  ref (gensym "str") 
      and interm_reg = ref (gensym "tmp") in
      Ptr( I8), (Id !ret_reg),
    List.rev [
      G (!global_id, (Array (String.length str + 1, I8),  GString str)); 
      E (!ret_reg, Alloca (Ptr(I8))); 
      I (!interm_reg,  Bitcast(Ptr (Array (String.length str + 1, I8)),Gid !global_id,Ptr (I8) )) ;
      I (gensym "tmp", Store (Ptr(I8), Id !interm_reg, Id !ret_reg))
    ] 
    
  | Bop (binop, exp1, exp2) -> 
    let ll_ty1, op1, stream1 =  cmp_exp c exp1
    and ll_ty2, op2, stream2 =  cmp_exp c  exp2 in 
     let ret_ty, binop_instr_stream = cmp_bop binop (Id !op1_reg) (Id !op2_reg) !res_reg
    in

    ret_ty, (Id !ret_reg) , (stream1 >@ stream2 >@ 
    
    [E (!ret_reg, Alloca ret_ty)] >@
    lift [(!op1_reg, Load (Ptr ll_ty1, op1) ); (!op2_reg, Load ( Ptr ll_ty2, op2))] >@
    binop_instr_stream >@ 
    lift [(gensym "tmp", Store (ret_ty, (Id !res_reg), (Id !ret_reg)))]
    )

  | Uop (unop,exp) -> 
    let ll_ty, op, stream = cmp_exp c exp in 
    let ret_ty, unop_instr_stream = cmp_uop unop (Id !op1_reg) !res_reg
  in
    ret_ty, Id !ret_reg, (stream >@

    [E (!ret_reg, Alloca ret_ty)] >@
    [I(!op1_reg, Load(Ptr ll_ty, op))] >@
    unop_instr_stream >@ 
    [I(gensym "tmp", Store (ret_ty, (Id !res_reg), (Id !ret_reg)))]
    )
  | Id id -> 
      let ll_ty, ll_op = Ctxt.lookup id c in
      ll_ty, ll_op, []
  | Call (f_expr, args) -> 
      let ptr_ty, op_ptr, stream_ptr = cmp_exp c f_expr
      and cmped_args = List.map (fun exp -> cmp_exp c exp) args in
      let ty_id_load  = List.map (fun (ty, op, _ ) -> let arg_reg = ref (gensym "arg") in (ty, (Ll.Id !arg_reg), I(!arg_reg, Load (Ptr ty, op)) )) cmped_args
  in 
    let call_args = List.map (fun (ty, op, _) -> (ty, op)) ty_id_load 
    and stream_load = List.map (fun (_, _, inst) -> inst) ty_id_load in

    let fin_stream = stream_ptr >@ 
    (List.concat_map (fun (_, _, stream) -> stream) cmped_args) >@
    stream_load >@
    [
      I( !ret_reg, Call (ptr_ty, op_ptr,call_args))
    ]
  in
    let Ptr(fin_ty) = ptr_ty in
    fin_ty, (Id !ret_reg), fin_stream
      

(* Compile a statement in context c with return type rt. Return a new context, 
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)

let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  let {elt = stmt} = stmt
  and rhs_reg = ref (gensym "rhs")
  and var_reg = ref (gensym "var")
  and ret_reg = ref (gensym "ret_function")
  and cmp_stmt_list c ret_ty stmt_l = List.fold_left (fun (c, code) s ->
        let c, new_stream = cmp_stmt c rt s in
        (c, code >@ new_stream)) (c, []) stmt_l


 in
  match stmt with
  | Ret None -> c , [T (Ret (Void, None))]
  | Ret Some (exp_node) -> 
    let ll_ty, op, stream = cmp_exp c exp_node in 
    c, stream >@ lift [(!ret_reg, Load (Ptr(ll_ty), op))] >@ [T (Ret( ll_ty, Some (Id !ret_reg)))]
    
  | Assn (exp1, exp2) -> 
      let ll_ty_lhs, op_lhs, stream_lhs = cmp_exp c exp1
      and ll_ty_rhs, op_rhs, stream_rhs = cmp_exp c exp2
  in
      let stream_load_rhs = lift [(!rhs_reg, Load (Ptr ll_ty_rhs, op_rhs)); (gensym "tmp", Store( ll_ty_lhs, (Id !rhs_reg), op_lhs))] 
  in
   (c, stream_lhs >@ stream_rhs >@ stream_load_rhs)
   
  | Decl (id, exp1) -> 
    let ll_ty_rhs, op_rhs, stream_rhs = cmp_exp c exp1 in
    let stream  = stream_rhs >@ [E (!var_reg, Alloca ll_ty_rhs)] >@ lift [(!rhs_reg, Load(Ptr(ll_ty_rhs), op_rhs)) ; (gensym "tmp", Store(  ll_ty_rhs, (Id !rhs_reg), (Id !var_reg)))] in
    let new_c = Ctxt.add c id (ll_ty_rhs, (Id !var_reg)) in
    (new_c, stream) 
    
  | SCall (f_expr, args) -> 
      let ptr_ty, op_ptr, stream_ptr = cmp_exp c f_expr
      and cmped_args = List.map (fun exp -> cmp_exp c exp) args in
      let ty_id_load  = List.map (fun (ty, op, _ ) -> let arg_reg = ref (gensym "arg") in (ty, (Ll.Id !arg_reg), I(!arg_reg, Load (Ptr ty, op)) )) cmped_args
    in 
    let call_args = List.map (fun (ty, op, _) -> (ty, op)) ty_id_load 
    and stream_load = List.map (fun (_, _, inst) -> inst) ty_id_load in

    let fin_stream = stream_ptr >@ 
    (List.concat_map (fun (_, _, stream) -> stream) cmped_args) >@
    stream_load >@
    [
      let Ptr(Fun(_, ret_ty)) = ptr_ty in
      I( !ret_reg, Call (ret_ty, op_ptr,call_args))
    ]
  in
    c, fin_stream
    
  | While (expr, stmt_l) ->
    let lbl_test = ref (gensym "test")
    and lbl_body = ref (gensym "body")
    and lbl_fin = ref (gensym "fin")
  in
    let ll_ty, op, stream_condition = cmp_exp c expr 
    and c_new, body_stream = cmp_stmt_list c rt stmt_l
    in
    c, (
    
      [T(Br !lbl_test) ] >@  

    [L( !lbl_test)] >@
      stream_condition >@
      lift [(!var_reg, Load(Ptr(ll_ty), op))] >@
      [T (Cbr ((Id !var_reg), !lbl_body, !lbl_fin) )] >@

    [L (!lbl_body)] >@
      body_stream >@
      [T(Br (!lbl_test))] >@
    
    [L(!lbl_fin)]
    )
  | If (exp, if_stmt_l, else_stmt_l) -> 
    let lbl_if = ref (gensym "if")
    and lbl_else = ref (gensym "else")
    and lbl_fin = ref (gensym "fin")
  in
    let ll_ty, op_condition, stream_condition = cmp_exp c exp
    and c_if, stream_if = cmp_stmt_list c rt if_stmt_l
    and c_else, stream_else = cmp_stmt_list c rt else_stmt_l 
  in
    c, (stream_condition >@
    lift [(!var_reg, Load (Ptr ll_ty, op_condition))] >@
    [T (Cbr ((Id !var_reg), !lbl_if, !lbl_else))] >@
    [L(!lbl_if)] >@
      stream_if >@
      [T(Br !lbl_fin)] >@
    [L(!lbl_else)] >@
      stream_else >@
      [T(Br !lbl_fin)] >@
    [L(!lbl_fin)])

  |For (decl_l, cond_exp, inc_stmt, body_stmt_l) ->
    let decl_l = List.map (fun vd -> {elt = Decl vd ; loc = ("frontend.ml", (69, 420), (69,420) )}) decl_l
  in
    let condition_expr =
      begin match cond_exp with
      | None -> {elt = (CBool true); loc = ("frontend.ml", (69, 420), (69,420))}
      | Some exp -> exp
    end
    and while_body_stmt_l = 
      begin match inc_stmt with
      | None -> body_stmt_l
      | Some inc_stmt -> body_stmt_l @ [inc_stmt] 
      end
    in
       let for_c, stream = cmp_stmt_list c rt @@ decl_l @ [{elt = (Ast.While (condition_expr, while_body_stmt_l)); loc = ("frontend.ml", (69, 420), (69,420))}] 
    in
      (c, stream)
  | _ -> (c, [])
      
  

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : Ctxt.t * stream =
  List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts



(* Adds each function identifer to the context at an
   appropriately translated type.  

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c : Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
         let ft = TRef (RFun (List.map fst args, frtyp)) in
         Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p 

(* Populate a context with bindings for global variables 
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C). 
*)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  let ctxt_entry c decl = 
    match decl with
      | Gvdecl node -> 
        let {elt = gdecl} = node in 
          let {name = ast_id; init = exp_node} = gdecl in
            let  {elt = expr} = exp_node 
            and ll_id = gensym ast_id in
              begin match expr with
              | CNull rty -> Ctxt.add c  ast_id (cmp_rty rty, Gid ll_id) 
              | CBool b -> Ctxt.add c ast_id (cmp_ty TBool, Gid ll_id) 
              | CInt num -> Ctxt.add c ast_id (cmp_ty TInt, Gid ll_id) 
              | CStr str -> Ctxt.add c ast_id (Ptr I8, Gid ll_id)
              | CArr _ ->  Ctxt.add c ast_id (Ptr (Struct [I64; Array (0, I64)]) , Gid ll_id)
              | _ -> failwith "can only insert expr with constructors starting with C into global ctxt"
              end
      | Gfdecl _ -> c 
      in
      List.fold_left ctxt_entry c p

      


(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   4. Compile the body of the function using cmp_block
   5. Use cfg_of_stream to produce a LLVMlite cfg from 
 *)


let empty_cfg : cfg = ({insns= [] ; term = ("bogus_tmn", Br "bs")} , [])

let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =

  let cnt = ref 0 in


    let {elt = func_decl} = f in 

    let ll_param_tys = List.map (fun (ty, _)  -> cmp_ty ty) func_decl.args in
    let ll_uid_params = List.map (fun (_ , ast_id) -> gensym ast_id) func_decl.args in
    let new_c, minimal_stream = cmp_block c (cmp_ret_ty func_decl.frtyp) func_decl.body in
    let minimal_cfg, gdecls = cfg_of_stream minimal_stream in


    if !debug then print_endline @@ string_of_stream @@ List.rev minimal_stream else (); 
    {f_ty =  (ll_param_tys, (cmp_ret_ty func_decl.frtyp)); f_param = ll_uid_params ; f_cfg = minimal_cfg}, gdecls
  
  

(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations.
*)
let rec cmp_gexp c (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
    let {elt = exp} = e in
    match exp with
    | CNull rty -> (cmp_rty rty , GNull), []
    | CBool b -> (cmp_ty TBool, GInt (Int64.of_int @@ Bool.to_int b)), []
    | CInt num -> (cmp_ty TBool, GInt num), []
    | CArr _ -> failwith "array not implemented in cmp_gexp yet"
    | CStr _ -> failwith "str not implemented in cmp_gexp yet"
    | _ -> failwith "illegal typed passed to cmp_gexp"


(* Oat internals function context ------------------------------------------- *)
let internals = [
    "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
           let ll_gd, gs' = cmp_gexp c gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }
