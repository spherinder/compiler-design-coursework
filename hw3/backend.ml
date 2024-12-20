(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understanding this entire file and
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page.
*)

let debug_backend = ref false 

(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge



(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be represented
   in 64 bit. This greatly simplifies code generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m

let todo() = failwith "todo"

(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip).

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).
*)
let compile_operand (layout:layout) (dest:X86.operand) : Ll.operand -> ins = function
  | Null -> Movq, [Imm (Lit 0L); dest]
  | Const x -> Movq, [Imm (Lit x); dest]
  | Id id -> Movq, [lookup layout id; dest]
  | Gid gid -> Leaq, [Ind3 (Lbl (Platform.mangle gid), Rip); dest]

(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)

let rec drop (i : int) (l : 'a list) : 'a list =
  if i <= 0 then l
  else match l with
    | [] -> []
    | _::xs -> drop (i-1) xs

let rec take (i : int) (l : 'a list) : 'a list =
  if i <= 0 then []
  else match l with
    | [] -> []
    | x::xs -> x::take (i-1) xs

let compile_call (layout:layout) (dest:X86.operand) (op : Ll.operand) (args: (ty * Ll.operand) list) : X86.ins list =
  let regargs, stackargs = take 6 args, drop 6 args in
  let regs = take (List.length regargs) [Rdi; Rsi; Rdx; Rcx; R08; R09] in
  let stacksize = List.length stackargs * 8 in
  (*register arguments*)
  List.map2 (fun reg (_,arg)-> compile_operand layout (Reg reg) arg) regs regargs
  (*stack arguments*)
  @ [Subq, [Imm (Lit (Int64.of_int stacksize)); Reg Rsp]]
  @ (List.flatten @@ List.mapi (fun i (_,arg)->
      [ compile_operand layout (Reg R10) arg
      ; Movq, [Reg R10; Ind3 (Lit (Int64.of_int (i * 8)), Rsp)]
      ]
    ) stackargs)
  (*call*)
  @ [compile_operand layout (Reg R10) op
    ; Callq, [Reg R10]
    ; Movq, [Reg Rax; dest]
    ]



(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelementptr, you must generate x86 code that performs
   the appropriate arithmetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes.
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty (tdecls:(tid * ty) list) : Ll.ty -> int = function
  | Void | I8 | Fun _ -> 0
  | I1 | I64 | Ptr _ -> 8
  | Struct ts -> List.fold_left (+) 0 @@ List.map (size_ty tdecls) ts
  | Array (i, t) -> i * size_ty tdecls t
  | Namedt tid -> size_ty tdecls @@ lookup tdecls tid


(* Generates code that computes a pointer value.

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

     - if t is a struct, the index must be a constant n and it
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the
       sizes of the types of the previous elements ]

     - if t is an array, the index can be any operand, and its
       value determines the offset within the array.

     - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)
let rec scanl (f : 'b -> 'a -> 'b) (q : 'b) (ls : 'a list) : 'b list = q :: (match ls with
    | [] -> []
    | x::xs -> scanl f (f q x) xs)

let compile_gep ({layout; tdecls}:ctxt) (ptr_t,ptr : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
  let inner_ptr_t = match ptr_t with
    | Ptr (Namedt lbl) -> lookup tdecls lbl (*TODO is this necessary?*)
    | Ptr t -> t
    | _ -> raise (Failure "gep takes a pointer")
  in
  let get_ty (t:Ll.ty) (op:Ll.operand) : Ll.ty =
    match t,op with
    | Struct ts, Const m -> List.nth ts @@ Int64.to_int m
    | Array (_, t), _ -> t
    | _ -> raise (Failure "gep only indexes into structs and arrays")
  in
  (* `take` drops last element *)
  let path_ts = take (List.length path) @@ scanl get_ty inner_ptr_t path in

  [compile_operand layout (Reg Rax) ptr]
  @ (List.flatten @@ List.map2 (fun (t:Ll.ty) (op:Ll.operand) ->
      match t, op with
      | Struct ts, Const m ->
        let offset = List.fold_left (+) 0 @@ List.map (size_ty tdecls) @@ take (Int64.to_int m) ts in
        [Addq, [Imm (Lit (Int64.of_int offset)); Reg Rax]]
      | Array (_, elem_t), _ ->
        let elemsize = size_ty tdecls elem_t in
        [ compile_operand layout (Reg Rcx) op
        ; Imulq, [Imm (Lit (Int64.of_int elemsize)); Reg Rcx]
        ; Addq, [Reg Rcx; Reg Rax]
        ]
      | _ -> raise (Failure "won't happen")
    ) path_ts path)


(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let compile_insn ({tdecls; layout} as ctxt:ctxt) ((uid:uid), (i:Ll.insn)) : X86.ins list =
  let dest = lookup layout uid in
  match i with
  | Binop (bop, t, op1, op2) ->
    (match t with I64 -> () | _ -> raise (Failure "binop type must be i64"));
    [ compile_operand layout (Reg Rax) op1
    ; compile_operand layout (Reg Rcx) op2
    ] @
    (match bop with
     | Add -> [Addq, [Reg Rax; Reg Rcx]]
     | Sub -> [Subq, [Reg Rax; Reg Rcx]]
     | Mul -> [Imulq, [Reg Rax; Reg Rcx]]
     | Shl -> [Shlq, [Reg Rax; Reg Rcx]]
     | Lshr -> [Shrq, [Reg Rax; Reg Rcx]]
     | Ashr -> [Sarq, [Reg Rax; Reg Rcx]]
     | And -> [Andq, [Reg Rax; Reg Rcx]]
     | Or -> [Orq, [Reg Rax; Reg Rcx]]
     | Xor -> [Xorq, [Reg Rax; Reg Rcx]]
    ) @
    [Movq, [Reg Rcx; dest]]
  | Alloca t ->
    [Subq, [Imm (Lit (Int64.of_int (size_ty tdecls t))); Reg Rsp]] @
    [Movq, [Reg Rsp; dest]]
  | Load (_, op) ->
    [compile_operand layout (Reg Rax) op] @
    [Movq, [Ind2 Rax ; Reg Rcx]] @
    [Movq, [Reg Rcx; dest]]
  | Store (_, srcop, dstop) ->
    [compile_operand layout (Reg Rax) srcop] @
    [compile_operand layout (Reg Rcx) dstop] @
    [Movq , [Reg Rax; Ind2 Rcx]]
  | Icmp (cnd, _, op1, op2) ->
    [ Movq, [Imm (Lit 0L); dest]
    ; compile_operand layout (Reg Rax) op1
    ; compile_operand layout (Reg Rcx) op2
    ; Cmpq, [Reg Rax; Reg Rcx]
    ; Set (compile_cnd cnd), [dest]
    ]
  | Call (_, op, args) -> compile_call layout dest op args
  | Bitcast (_, op, _) ->
    [ compile_operand layout (Reg Rax) op
    ; Movq, [Reg Rax; dest]
    ]
  | Gep (t, op, ops) ->
    compile_gep ctxt (t,op) ops
    @ [Movq, [Reg Rax; dest]]




(* compiling terminators  --------------------------------------------------- *)

(* prefix the function name [fn] to a label to ensure that the X86 labels are
   globally unique . *)
let mk_lbl (fn:string) (l:string) = fn ^ "." ^ l

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional

   [fn] - the name of the function containing this terminator
*)
let compile_terminator (fn:string) (ctxt:ctxt) (t:Ll.terminator) : ins list =
  let stack_teardown = (Movq, [Reg Rbp; Reg Rsp]) :: (Popq, [Reg Rbp] )::(Retq, [])::[]
  and compile_mov src_ll dest_86 = compile_operand ctxt.layout dest_86 src_ll
  and ind1_of_lbl lbl = Ind1(Lbl (mk_lbl fn lbl))
  in
  match t with
  | Ret (_, None) -> stack_teardown
  | Ret (_, Some res) ->   [compile_mov res (Reg Rax)] @ stack_teardown
  | Br label -> [(Jmp, [ind1_of_lbl label])]
  | Cbr (op, label1, label2) -> [compile_mov op (Reg R08); (Cmpq, [Imm (Lit 0L); Reg R08]);
                                  ((J Neq), [ind1_of_lbl label1]); (J Eq, [ind1_of_lbl label2])]



(* compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete.
   [fn] - the name of the function containing this block
   [ctxt] - the current context
   [blk]  - LLVM IR code for the block
*)
let compile_block (fn:string) (ctxt:ctxt) ({insns; term = (_,term)}:Ll.block) : ins list =
  List.concat_map (compile_insn ctxt) insns @ compile_terminator fn ctxt term


let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)


(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : operand =
    match n with
    | 0 -> Reg Rdi
    | 1 -> Reg Rsi
    | 2 -> Reg Rdx
    | 3 -> Reg Rcx
    | 4 -> Reg R08
    | 5 -> Reg R09
    | i -> Ind3( Lit(Int64.of_int @@ ((i + 1 - 7) + 2) * 8), Rbp) (*formula is ((i - 7) + 2)*8 (ref. lecture slides). +1 is because indexing here starts from 0*)



(* We suggest that you create a helper function that computes the
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id
     is also stored as a stack slot.
   - see the discussion about locals

*)


let create_layout_entry uid offset =  (uid, (Ind3 (Lit( Int64.of_int offset), Rbp)))


let stack_layout (args : uid list) ((block, lbled_blocks):cfg) : layout =
  let args_layout = List.mapi (fun n u ->  create_layout_entry u (-(n+1) * 8) ) args in

    let cntr =  ref (List.length args_layout) in (* this is suspicious*)

    let layout_block ({insns = insns; term = (tuid, term)}:block):layout =

      let block_layout_init = List.mapi ( fun  n (u, instr) ->  create_layout_entry u (-(n + 1 + !cntr)*8 ) ) insns in
        let _ = cntr:= !cntr + List.length insns + 1 in

            let block_layout_fin = block_layout_init @ [( create_layout_entry tuid (!cntr * -8 ))] in (* putting the terminator result onto stack slot. May be unnecessary*)
                    block_layout_fin
          in
          args_layout @ (layout_block block) @ (List.concat_map (fun (lbl, blk) -> layout_block blk )  lbled_blocks)





(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl (tdecls:(tid * ty) list) (name:string) ({ f_ty; f_param; f_cfg }:fdecl) : prog =

  let layout = stack_layout f_param f_cfg in
    let ctxt = {tdecls = tdecls; layout = layout} in
      let stack_setup = (Pushq, [Reg Rbp]) :: (Movq, [Reg Rsp; Reg Rbp])::[]
      and arguments_alloc =
          List.mapi (fun inx u -> (Movq , [(arg_loc inx);  lookup layout u]))  f_param  (* optimize *)
      and first_block = compile_block name ctxt (fst f_cfg)
      and tail_blocks = List.map (fun (lbl, blk) -> compile_lbl_block name lbl ctxt blk) (snd f_cfg)
    in
    if !debug_backend then
      layout |> List.map (fun (u, instr) -> u ^ (string_of_operand instr)) |> String.concat "\n" |> String.cat "layout: \n"  |> print_endline
      else ();

    [Asm.text (Platform.mangle name) (stack_setup @ arguments_alloc @ first_block)] @ tail_blocks
(* compile_gdecl ------------------------------------------------------------ *)
              (*TODO: give functions names*)
(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (t1,g,t2) -> compile_ginit g

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
