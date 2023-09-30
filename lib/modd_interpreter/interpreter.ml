open! Core
open! Modd_parsing
open! Modd_typing
open! Ast_types
open! Types

module List = struct
  include List

  let set_nth_exn t i x =
    if i >= length t then raise_s [%message "List.update_exn: index out of bounds"];
    List.mapi t ~f:(fun j y -> if i = j then x else y)
  ;;
end

module Address = struct
  type location =
    | Stack
    | Heap

  let location_of_mode : Types.mode -> location = function
    | Tmod_local -> Stack
    | Tmod_global -> Heap
  ;;

  type t =
    { addr_loc : location
    ; addr : int
    }
end

module Value = struct
  type t =
    | Abstraction of
        { env : Address.t
        ; type_vars : type_var list
        ; body : Typedtree.expression
        }
    | Closure of
        { env : Address.t
        ; param : Typedtree.pattern
        ; body : Typedtree.expression
        }
    | Constant of constant
    | Address of Address.t

  let unit = Constant Const_unit
  let int n = Constant (Const_int n)
end

module Env = struct
  type t =
    { values : Value.t String.Map.t
    ; types : Types.type_expr Type_var.Map.t
    }

  let empty = { values = String.Map.empty; types = Type_var.Map.empty }
  let add t ~var ~value = { t with values = Map.set t.values ~key:var ~data:value }

  let add_type t ~type_var ~type_expr =
    { t with types = Map.set t.types ~key:type_var ~data:type_expr }
  ;;

  let find t var = Map.find_exn t.values var
  let find_type t tvar = Map.find_exn t.types tvar
end

module Box = struct
  type t =
    | Tuple of Value.t list
    | Variant of string * Value.t option
    | Ref of Value.t
end

module Memory = struct
  type block =
    | Boxed of Box.t
    | Env of Env.t

  type t =
    { stack : block list
    ; heap : block list
    }

  let empty = { stack = []; heap = [] }

  let get t ({ addr; addr_loc } : Address.t) =
    match addr_loc with
    | Stack -> List.nth_exn t.stack addr
    | Heap -> List.nth_exn t.heap addr
  ;;

  let set t ({ addr; addr_loc } : Address.t) block =
    match addr_loc with
    | Stack -> { t with stack = List.set_nth_exn t.stack addr block }
    | Heap -> { t with stack = List.set_nth_exn t.heap addr block }
  ;;

  let alloc t (loc : Address.location) block =
    match loc with
    | Stack ->
      ( { t with stack = t.stack @ [ block ] }
      , Address.{ addr = List.length t.stack; addr_loc = loc } )
    | Heap ->
      ( { t with stack = t.heap @ [ block ] }
      , Address.{ addr = List.length t.heap; addr_loc = loc } )
  ;;
end

type state =
  { env : Env.t
  ; memory : Memory.t
  }

type value = Value.t

let rec match_pattern ~state (value : value) (pat : Typedtree.pattern) : state option =
  let open Option.Let_syntax in
  match value, pat.pat_desc with
  | _, Tpat_any -> return state
  | value, Tpat_var var -> return { state with env = Env.add state.env ~var ~value }
  | Constant const, Tpat_const const' when equal_constant const const' -> return state
  | Address addr, pat ->
    (match Memory.get state.memory addr, pat with
    | Boxed (Tuple values), Tpat_tuple pats ->
      (match
         List.fold2 values pats ~init:(return state) ~f:(fun state value pat ->
             let%bind state = state in
             match_pattern ~state value pat)
       with
      | Unequal_lengths -> None
      | Ok result -> result)
    | Boxed (Variant (tag, Some value)), Tpat_construct (constr, Some pat)
      when String.(constr.constructor_name = tag) -> match_pattern ~state value pat
    | Boxed (Variant (tag, None)), Tpat_construct (constr, None)
      when String.(constr.constructor_name = tag) -> return state
    | _ -> None)
  | _ -> None
;;

let eval_constant (const : constant) : value =
  match const with
  | Const_unit -> Value.unit
  | Const_int n -> Value.int n
;;

let eval_unary_op ~state (loc : Address.location) (uop : unary_op) (arg : value)
    : state * value
  =
  match uop, arg with
  | Uop_ref, arg ->
    let memory, ref_addr = Memory.alloc state.memory loc (Boxed (Ref arg)) in
    { state with memory }, Address ref_addr
  | Uop_deref, Address ref_addr ->
    (match Memory.get state.memory ref_addr with
    | Boxed (Ref value) -> state, value
    | _ -> raise_s [%message "eval_unary_op: expected ref"])
  | _ -> raise_s [%message "eval_unary_op: type error"]
;;

let eval_binary_op ~state (bop : binary_op) (arg1 : value) (arg2 : value) : state * value =
  let int_of_value = function
    | Value.Constant (Const_int n) -> n
    | _ -> raise_s [%message "eval_binary_op: expected int"]
  in
  match bop, arg1, arg2 with
  | Bop_add, arg1, arg2 -> state, Value.int @@ (int_of_value arg1 + int_of_value arg2)
  | Bop_sub, arg1, arg2 -> state, Value.int @@ (int_of_value arg1 - int_of_value arg2)
  | Bop_mul, arg1, arg2 -> state, Value.int @@ (int_of_value arg1 * int_of_value arg2)
  | Bop_assign, Address ref_addr, arg2 ->
    let memory = Memory.set state.memory ref_addr (Boxed (Ref arg2)) in
    { state with memory }, Value.unit
  | _, _, _ -> raise_s [%message "eval_binary_op: type error"]
;;

let rec eval_value_binding ~state (vb : Typedtree.value_binding) : state =
  let state, exp_value = eval_abstraction ~state vb.tvb_expr in
  match match_pattern ~state exp_value vb.tvb_pat with
  | None -> raise_s [%message "eval_value_binding: match error"]
  | Some state -> state

and eval_abstraction ~state (type_vars, body) : state * value =
  match type_vars with
  | [] -> eval ~state body
  | type_vars ->
    let loc = Address.location_of_mode body.exp_mode in
    let memory, env_addr = Memory.alloc state.memory loc (Env state.env) in
    { state with memory }, Abstraction { env = env_addr; type_vars; body }

and eval_case ~state matchee (case : Typedtree.case) : (state * value) option =
  let open Option.Let_syntax in
  let%map state = match_pattern ~state matchee case.tc_lhs in
  eval ~state case.tc_rhs

and eval_match ~state matchee cases =
  let rec loop = function
    | [] -> raise_s [%message "eval_match: match error"]
    | case :: cases ->
      (match eval_case ~state matchee case with
      | Some result -> result
      | None -> loop cases)
  in
  loop cases

and eval_instance ~state (value : value) type_exprs : state * value =
  match value, type_exprs with
  | value, [] -> state, value
  | Abstraction { env; type_vars; body }, type_exprs ->
    (match Memory.get state.memory env with
    | Env env ->
      (match
         List.fold2 type_vars type_exprs ~init:env ~f:(fun env type_var type_expr ->
             Env.add_type env ~type_var ~type_expr)
       with
      | Unequal_lengths -> raise_s [%message "eval_instance: type error"]
      | Ok env -> eval ~state:{ state with env } body)
    | _ -> raise_s [%message "eval_instance: type error"])
  | _ -> raise_s [%message "eval_instance: type error"]

and eval ~state (exp : Typedtree.expression) : state * value =
  let loc = Address.location_of_mode exp.exp_mode in
  match exp.exp_desc with
  | Texp_var (var, type_exprs) ->
    let value = Env.find state.env var in
    eval_instance ~state value type_exprs
  | Texp_const const -> state, Constant const
  | Texp_uop (uop, arg) ->
    let state, arg_value = eval ~state arg in
    eval_unary_op ~state loc uop arg_value
  | Texp_bop (bop, arg1, arg2) ->
    let state, arg1_value = eval ~state arg1 in
    let state, arg2_value = eval ~state arg2 in
    eval_binary_op ~state bop arg1_value arg2_value
  | Texp_let (vbs, in_) ->
    let state =
      List.fold vbs ~init:state ~f:(fun state vb -> eval_value_binding ~state vb)
    in
    eval ~state in_
  | Texp_fun (param, body) ->
    let memory, env_addr = Memory.alloc state.memory loc (Env state.env) in
    ( { state with memory }
    , Value.Closure { env = env_addr; param = param.tparam_pat; body } )
  | Texp_app (func, arg) ->
    let state, func_value = eval ~state func in
    let state, arg_value = eval ~state arg in
    (match func_value with
    | Closure { env; param; body } ->
      (match Memory.get state.memory env with
      | Env env ->
        let state =
          match match_pattern ~state:{ state with env } arg_value param with
          | None -> raise_s [%message "eval: match error"]
          | Some state -> state
        in
        eval ~state body
      | _ -> raise_s [%message "eval: expected `Env`"])
    | _ -> raise_s [%message "eval: expected `Closure`"])
  | Texp_tuple exps ->
    let state, values =
      List.fold_map exps ~init:state ~f:(fun state exp -> eval ~state exp)
    in
    let memory, tuple_addr = Memory.alloc state.memory loc (Boxed (Tuple values)) in
    { state with memory }, Address tuple_addr
  | Texp_construct (constr, arg) ->
    let state, arg_value =
      (* No `Option.fold_map` :( *)
      match arg with
      | None -> state, None
      | Some arg ->
        let state, arg_value = eval ~state arg in
        state, Some arg_value
    in
    let memory, variant_addr =
      Memory.alloc state.memory loc (Boxed (Variant (constr.constructor_name, arg_value)))
    in
    { state with memory }, Address variant_addr
  | Texp_match (matchee, cases) ->
    let state, matchee_value = eval ~state matchee in
    eval_match ~state matchee_value cases
;;
