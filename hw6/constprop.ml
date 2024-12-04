open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"

    
  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let fold (binop: bop) (a: int64) (b:int64) =
  match binop with
  | Add -> Int64.add a b 
  | Sub -> Int64.sub a b
  | Mul -> Int64.mul a b
  | Shl -> Int64.shift_left a (Int64.to_int b)
  | Lshr -> Int64.shift_right_logical a (Int64.to_int b)
  | Ashr -> Int64.shift_right a (Int64.to_int b)
  | And -> Int64.logand a b 
  | Or -> Int64.logor a b 
  | Xor -> Int64.logxor a b 


let rec fold_icmp cnd a b =
  let comp = Int64.compare a b in

  match cnd with
  | Eq -> if comp = 0 then 1L else 0L
  | Ne -> if comp = 0 then 1L else 0L 
  | Slt -> if comp < 0 then 1L else 0L
  | Sle -> if  comp <= 0 then 1L else 0L
  | Sgt -> if comp > 0 then 1L else 0L
  | Sge -> if comp >= 0 then 1L else 0L

  (* dfa: lbl -> (uid to SymConst mapping)*) (*each lbl has an associated map for all of its uids*)
  (*fact: uid -> symbols mapping*)
(*fact here stays for the meet of all incoming data flows (an operand is constant only if it is const for every
  incoming edge)    *)

let lookup_operand (op:Ll.operand) (d:fact): SymConst.t = 
  let open SymConst in
    match op with
    | Const a -> Const a 
    | Id uid -> UidM.find_or UndefConst d uid 
    | _ -> UndefConst

let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let open SymConst in
  
  let update_fact u sym =  UidM.update_or UndefConst (fun _ -> sym) u d in 

  match i with
  | Binop (bop, ty, op1, op2) -> 
    begin match lookup_operand op1 d, lookup_operand op2 d with
    |  (Const a), (Const b) -> let folded_c = fold bop a b in 
                                      update_fact u (Const folded_c) 
    | UndefConst, _ | _, UndefConst -> update_fact u UndefConst 
    |  NonConst, _  | _ , NonConst -> update_fact u NonConst
    end
  | Icmp (cnd, ty, op1, op2) ->
    begin match lookup_operand op1 d, lookup_operand op2 d with
    | (Const a), (Const b) -> 
      let folded_c = fold_icmp cnd a b in 
                                      update_fact u (Const folded_c) 
    | UndefConst, _ | _, UndefConst -> update_fact u UndefConst 
    |  NonConst, _  | _ , NonConst -> update_fact u NonConst
    end
  | Store _ | Call _ -> update_fact u UndefConst
  | _ -> update_fact u NonConst 
       

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  = 
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact = 
      let open SymConst in

      let union f1 f2 = 
        let func key val1 val2 = 
          match val1, val2 with
          | (Some (Const a)), (Some (Const b)) -> if (Int64.compare a b) = 0 then Some (Const a) else Some NonConst 
          | (Some (UndefConst)), _ | _, (Some (UndefConst)) -> Some UndefConst
          | (Some (NonConst)), _ | (_, Some(NonConst)) -> Some NonConst 
          | _, _ -> Some UndefConst
        in
          UidM.merge func f1 f2
      in

      if List.length ds = 0 
        then UidM.empty 
      else if List.length ds = 1 
        then List.hd ds 
      else  
        List.fold_left union (List.hd ds) (List.tl ds)

  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right 
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)

(*function to be used in conjunction map operations*)

let subst_arg fact (ty, op) = 
          let subst = lookup_operand op fact in
          begin match subst with
          | Const a -> (I64, Ll.Const a)
          | _ -> (ty, op)
          end

let subst_op fact op = 
  let subst = lookup_operand op fact in
  begin match subst with 
  | Const a -> Ll.Const a
  | _ -> op 
  end

let fold_constants (fact: fact) ( i: Ll.insn): Ll.insn = 
  
  match i with
  | Binop (bop, ty, op1, op2) as instr ->
          let subst1 = lookup_operand op1 fact 
          and subst2 = lookup_operand op2 fact in
          
          begin match subst1, subst2 with
          | (Const a), (Const b) -> Binop (bop, ty, Ll.Const a, Ll.Const b)
          | (Const a), _ -> Binop (bop, ty, Ll.Const a, op2)
          | _, (Const b) -> Binop (bop,ty, op1, Ll.Const b)
          | _, _ -> instr
          end
  | Store (ty, op1, ptr) as instr -> 
          let subst1 = lookup_operand op1 fact in
          begin match subst1 with
          | (Const a) -> Store (ty, Ll.Const a, ptr)
          | _ -> instr 
          end
  | Icmp (cnd, ty, op1, op2) as instr->
          let subst1 = lookup_operand op1 fact
          and subst2 = lookup_operand op2 fact in
          
          begin match subst1, subst2 with
          | (Const a), (Const b) -> Icmp (cnd, ty, Ll.Const a, Ll.Const b)
          | (Const a), _ -> Icmp (cnd, ty, Ll.Const a, op2)
          | _, (Const b) -> Icmp (cnd, ty, op1, Ll.Const b)
          | _, _ -> instr
          end

  | Call (ty, f_op, arg_l) ->
      let arg_l' = List.map (subst_arg fact) arg_l in
            Call(ty, f_op, arg_l')

  | Gep (ptr_ty, ptr, op_l) -> 
    let op_l' = List.map (subst_op fact) op_l in 
      Gep(ptr_ty, ptr, op_l') 

  | _ -> i 
  

let fold_term (fact:fact) (t: Ll.terminator) : Ll.terminator = 
  match t with
  | Ret (ty, (Some op1)) ->
      let subst1 = lookup_operand op1 fact in
      begin match subst1 with 
      | (Const a) -> Ret (I64, Some (Ll.Const a))
      |  _ -> t
      end 
  | Cbr (op1, lbl1, lbl2) as term ->
      let subst1 = lookup_operand op1 fact in
      begin match subst1 with
      | (Const a) -> Cbr (Ll.Const a, lbl1, lbl2)
      | _ -> t
      end
  | _ -> t 

(*cg - output of analyze*)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in
  
  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in (*b: Ll.block = *)
    let cb = Graph.uid_out cg l in (*cb: uid -> Fact.t as in cg*)
    (*go through all instructions in the block and substitute with ready constants*)

    let ins_stream = b.insns 
    and (t_uid, term_expr) = b.term in 

    let folded_stream = List.map (fun (u, i) -> (u, fold_constants (cb u) i)) ins_stream
    and folded_term = fold_term (cb t_uid) term_expr  in

    let new_block = {insns = folded_stream; term = t_uid, folded_term} in
    
    let updated_blocks = LblM.update (fun _ -> new_block) l cfg.blocks in

    {blocks = updated_blocks; preds = cfg.preds; ret_ty = cfg.ret_ty; args = cfg.args}
    
  in
  LblS.fold cp_block (Cfg.nodes cfg) cfg

   (* Cfg.nodes : LblS , cfg - acc, LblS.fold  (elt -> acc -> acc) -> set -> acc *)