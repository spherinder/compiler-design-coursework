(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86
module O = Int64_overflow
(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next seven bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

let sbytearr_of_int64 (i:int64) : sbyte array =
  let open Char in
  let open Int64 in
  Array.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [|0; 8; 16; 24; 32; 40; 48; 56|]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = function
  | Eq -> fz
  | Neq -> not fz
  | Lt -> fs <> fo
  | Le -> fs <> fo || fz
  | Ge -> fs = fo
  | Gt -> fs = fo && not fz

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  let (-) = Int64.sub in
  if mem_bot <= addr && addr <= mem_top
  then Some (Int64.to_int (addr - mem_bot))
  else None

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)

let sign (x:quad) : bool = Int64.compare x 0L < 0

let (let*) opt f =
       match opt with
       | Some x -> f x
       | None -> raise X86lite_segfault

let readAddr ({flags;regs;mem}:mach) (x:quad) : quad =
  let* i = map_addr x in
  int64_of_sbytes @@ Array.to_list @@ Array.sub mem i 8

let readval ({flags;regs;mem}:mach) : operand -> quad = function
  | Imm (Lit x) -> x
  | Imm (Lbl _) | Ind1 (Lbl _) | Ind3(Lbl _, _) ->
    raise (Failure "no labels duh")
  | Reg r -> regs.(rind r)
  | Ind1 (Lit x) -> x
  | Ind2 r -> regs.(rind r)
  | Ind3 (Lit x, r) -> Int64.add x regs.(rind r)

let readind (m:mach) (opr:operand) : quad =
  match opr with
  | Ind1 _ | Ind2 _ | Ind3 _ -> readAddr m (readval m opr)
  | _ -> readval m opr

let writeDst ({flags;regs;mem} as m:mach) (dst:operand) (v:quad) : unit =
  match dst with
  | Reg r ->
    regs.(rind r) <- v
  | Ind1 _ | Ind2 _ | Ind3 _ ->
    let* i = map_addr (readval m dst) in
    Array.blit (sbytearr_of_int64 v) 0 mem i 8
  | Imm _ -> raise (Failure "can not write to constant")

let step ({ flags; regs; mem } as m : mach) : unit =
  let* rip = map_addr regs.(rind Rip) in
  let opc, oprs =
    match mem.(rip) with
    | InsB0 x -> x
    | _ -> raise (Failure "Invalid instruction")
  in
  regs.(rind Rip) <- Int64.add regs.(rind Rip) 8L;
  match opc, oprs with
  | Negq, [ dst ] ->
    let res = Int64.neg (readind m dst) in
    flags.fo <- res = Int64.min_int;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Addq, [ src; dst ] ->
    let d = readind m dst in
    let s = readind m src in
    let res = Int64.add d s in
    flags.fo <- sign d = sign s && sign res <> sign s;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Subq, [ src; dst ] ->
    let d = readind m dst in
    let s = readind m src in
    let res = Int64.sub d s in
    flags.fo <- sign d <> sign s && sign res = sign s;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Imulq, [ src; dst ] ->
    let { value = res; overflow = fo } : O.t = O.mul (readind m src) (readind m dst) in
    flags.fo <- fo;
    writeDst m dst res
  | Incq, [ dst ] ->
    let d = readind m dst in
    let res = Int64.add d 1L in
    flags.fo <- (not (sign d)) && sign res;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Decq, [ dst ] ->
    let d = readind m dst in
    let res = Int64.sub d 1L in
    flags.fo <- sign d && not (sign res);
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Notq, [ dst ] -> writeDst m dst @@ Int64.lognot (readind m dst)
  | Andq, [ src; dst ] ->
    let res = Int64.logand (readind m dst) (readind m src) in
    flags.fo <- false;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Orq, [ src; dst ] ->
    let res = Int64.logor (readind m dst) (readind m src) in
    flags.fo <- false;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Xorq, [ src; dst ] ->
    let res = Int64.logxor (readind m dst) (readind m src) in
    flags.fo <- false;
    flags.fz <- res = 0L;
    flags.fs <- sign res;
    writeDst m dst res
  | Sarq, [ src; dst ] -> begin
    match src with
    | Reg Rcx | Imm _ ->
      let s = readind m src in
      let res = Int64.shift_right (readind m dst) (Int64.to_int s) in
      if s <> 0L
      then begin
        if s = 1L then flags.fo <- false else ();
        flags.fz <- res = 0L;
        flags.fs <- sign res
      end
      else ();
      writeDst m dst res
    | _ -> raise (Failure "only shift with imm or rcx")
  end
  | Shlq, [ src; dst ] -> begin
    match src with
    | Reg Rcx | Imm _ ->
      let s = readind m src in
      let d = readind m dst in
      let res = Int64.shift_left d (Int64.to_int s) in
      if s <> 0L
      then begin
        if s = 1L then flags.fo <- sign d <> sign (Int64.shift_left d 1) else ();
        flags.fz <- res = 0L;
        flags.fs <- sign res
      end
      else ();
      writeDst m dst res
    | _ -> raise (Failure "only shift with imm or rcx")
  end
  | Shrq, [ src; dst ] -> begin
    match src with
    | Reg Rcx | Imm _ ->
      let s = readind m src in
      let d = readind m dst in
      let res = Int64.shift_right_logical d (Int64.to_int s) in
      if s <> 0L
      then begin
        flags.fs <- sign res;
        flags.fz <- res = 0L;
        if s = 1L then flags.fo <- sign d else ()
      end
      else ();
      writeDst m dst res
    | _ -> raise (Failure "only shift with imm or rcx")
  end
  | Set cnd, [ dst ] ->
    let cc = interp_cnd flags cnd in
    begin
      match dst with
      | Reg r ->
        regs.(rind r)
        <- (let masked = Int64.logand regs.(rind r) 0xFFFFFFFFFFFFFF00L in
            if cc then Int64.add masked 1L else masked)
      | Ind1 _ | Ind2 _ | Ind3 _ ->
        let* i = map_addr (readval m dst) in
        mem.(i) <- Byte (if cc then '\x01' else '\x00')
      | Imm _ -> raise (Failure "can not write to constant")
    end
  | Leaq, [ src; dst ] -> begin
    match src with
    | Ind1 _ | Ind2 _ | Ind3 _ -> writeDst m dst (readval m src)
    | _ -> raise (Failure "leaq only works on indirections")
  end
  | Movq, [ src; dst ] -> writeDst m dst (readind m src)
  | Pushq, [ src ] ->
    writeDst m (Reg Rsp) (Int64.sub (readind m (Reg Rsp)) 8L);
    writeDst m (Ind2 Rsp) (readind m src)
  | Popq, [ dst ] ->
    writeDst m dst (readind m (Ind2 Rsp));
    writeDst m (Reg Rsp) (Int64.add (readind m (Reg Rsp)) 8L)
  | Cmpq, [ src; dst ] ->
    let d = readind m dst in
    let s = readind m src in
    let res = Int64.sub d s in
    flags.fo <- sign d <> sign s && sign res <> sign d;
    flags.fz <- res = 0L;
    flags.fs <- sign res
  | Jmp, [ src ] -> writeDst m (Reg Rip) (readind m src)
  | Callq, [ src ] ->
    writeDst m (Reg Rsp) (Int64.sub (readind m (Reg Rsp)) 8L);
    writeDst m (Ind2 Rsp) (readind m (Reg Rip));
    writeDst m (Reg Rip) (readind m src)
  | Retq, [] ->
    writeDst m (Reg Rip) (readind m (Ind3 (Lit 0L, Rsp)));
    writeDst m (Reg Rsp) (Int64.add (readind m (Reg Rsp)) 8L)
  | J cnd, [ src ] ->
    if interp_cnd flags cnd then writeDst m (Reg Rip) (readind m src) else ()
  | _ -> raise (Failure "Wrong number of arguments probably")

let test_machine (bs : sbyte list) : mach =
  let mem = Array.make mem_size (Byte '\x00') in
  Array.blit (Array.of_list bs) 0 mem 0 (List.length bs) ;
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- mem_bot ;
  regs.(rind Rsp) <- Int64.sub mem_top 8L ;
  {flags= {fo= false; fs= false; fz= false}; regs; mem}

let (~$) i = Imm (Lit (Int64.of_int i))      (* int64 constants *)
let (~%) r = Reg r                           (* registers *)
let stack_offset (i: quad) : operand = Ind3 (Lit i, Rsp)

let mov_mr = test_machine [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag ;InsB0 (Movq, [~%Rax; stack_offset (-8L)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
let mov_mr2 = test_machine [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag ;InsB0 (Movq, [~%Rax; stack_offset (-8L)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
let mov_mr3 = test_machine [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag ;InsB0 (Movq, [~%Rax; stack_offset (-8L)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
(* let mov_mr = test_machine *)
(*   [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag *)
(*   ;InsB0 (Movq, [~%Rax; stack_offset (-8L)]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag] *)

let _ = step mov_mr2

let _ = step mov_mr3
(* let _ = step mov_mr3 *)

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
