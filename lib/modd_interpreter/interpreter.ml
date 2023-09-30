open! Import
open! Runtime

type state =
  { env : Env.t
  ; memory : Memory.t
  }

let realm_of_mode : Types.mode -> realm = function
  | Tmod_global -> Heap
  | Tmod_local -> Stack
;;

let rec match_pattern ~(state : state) (value : value) (pat : Typedtree.pattern)
    : Env.t option
  =
  let open Option.Let_syntax in
  let { env; memory } = state in
  match value, pat.pat_desc with
  | _, Tpat_any -> return env
  | value, Tpat_var var -> return (Env.add env ~var ~value)
  | Constant const, Tpat_const const' when equal_constant const const' -> return env
  | Address addr, pat ->
    (match Memory.deref state.memory addr, pat with
    | Tuple values, Tpat_tuple pats ->
      (match
         List.fold2 values pats ~init:(return env) ~f:(fun env value pat ->
             let%bind env = env in
             match_pattern ~state:{ env; memory } value pat)
       with
      | Unequal_lengths -> None
      | Ok result -> result)
    | Variant (tag, Some value), Tpat_construct (constr, Some pat)
      when String.(constr.constructor_name = tag) -> match_pattern ~state value pat
    | Variant (tag, None), Tpat_construct (constr, None)
      when String.(constr.constructor_name = tag) -> return env
    | _ -> None)
  | _ -> None
;;

let eval_constant (const : constant) : value = Constant const

let eval_unary_op ~state ~(realm : realm) (uop : unary_op) (arg : value) : value =
  match uop, arg with
  | Uop_ref, arg ->
    let addr = Memory.alloc state.memory (Ref arg) ~realm in
    Address addr
  | Uop_deref, Address ref_addr ->
    (match Memory.deref state.memory ref_addr with
    | Ref value -> value
    | _ -> raise_s [%message "Interpreter.eval_unary_op: expected ref"])
  | _ -> raise_s [%message "Interpreter.eval_unary_op: type error"]
;;

let eval_binary_op ~state (bop : binary_op) (arg1 : value) (arg2 : value) : value =
  let arith_bop =
    let int_of_value = function
      | Constant (Const_int n) -> n
      | _ -> raise_s [%message "Interpreter.eval_binary_op: expected int"]
    in
    let int_to_value n = Constant (Const_int n) in
    fun ~f v1 v2 -> int_to_value (f (int_of_value v1) (int_of_value v2))
  in
  match bop, arg1, arg2 with
  | Bop_add, arg1, arg2 -> arith_bop ~f:( + ) arg1 arg2
  | Bop_sub, arg1, arg2 -> arith_bop ~f:( - ) arg1 arg2
  | Bop_mul, arg1, arg2 -> arith_bop ~f:( * ) arg1 arg2
  | Bop_assign, Address ref_addr, arg2 ->
    Memory.assign state.memory ref_addr (Ref arg2);
    Constant Const_unit
  | _, _, _ -> raise_s [%message "Interpreter.eval_binary_op: type error"]
;;

let rec eval_value_binding ~state (vb : Typedtree.value_binding) : state =
  let exp_value = eval_abstraction ~state vb.tvb_expr in
  match match_pattern ~state exp_value vb.tvb_pat with
  | None -> raise_s [%message "Interpreter.eval_value_binding: match error"]
  | Some env -> { state with env }

and eval_abstraction ~state (type_vars, body) : value =
  match type_vars with
  | [] -> eval ~state body
  | type_vars ->
    let realm = realm_of_mode body.exp_mode in
    let env_addr = Memory.alloc state.memory (Env state.env) ~realm in
    Abstraction { env = env_addr; type_vars; body }

and eval_case ~state matchee (case : Typedtree.case) : value option =
  let open Option.Let_syntax in
  let%map env = match_pattern ~state matchee case.tc_lhs in
  eval ~state:{ state with env } case.tc_rhs

and eval_match ~state matchee cases =
  let rec loop = function
    | [] -> raise_s [%message "Interpreter.eval_match: match error"]
    | case :: cases ->
      (match eval_case ~state matchee case with
      | Some result -> result
      | None -> loop cases)
  in
  loop cases

and eval_instance ~state (value : value) type_exprs : value =
  match value, type_exprs with
  | value, [] -> value
  | Abstraction { env; type_vars; body }, type_exprs ->
    (match Memory.deref state.memory env with
    | Env env ->
      (match
         List.fold2 type_vars type_exprs ~init:env ~f:(fun env type_var type_expr ->
             Env.add_type env ~type_var ~type_expr)
       with
      | Unequal_lengths -> raise_s [%message "Interpreter.eval_instance: type error"]
      | Ok env -> eval ~state:{ state with env } body)
    | _ -> raise_s [%message "Interpreter.eval_instance: type error"])
  | _ -> raise_s [%message "Interpreter.eval_instance: type error"]

and eval ~state (exp : Typedtree.expression) : value =
  let realm = realm_of_mode exp.exp_mode in
  match exp.exp_desc with
  | Texp_var (var, type_exprs) ->
    let value = Env.find state.env var in
    eval_instance ~state value type_exprs
  | Texp_const const -> Constant const
  | Texp_uop (uop, arg) ->
    let arg_value = eval ~state arg in
    eval_unary_op ~state ~realm uop arg_value
  | Texp_bop (bop, arg1, arg2) ->
    let arg1_value = eval ~state arg1 in
    let arg2_value = eval ~state arg2 in
    eval_binary_op ~state bop arg1_value arg2_value
  | Texp_let (vbs, in_) ->
    let state =
      List.fold vbs ~init:state ~f:(fun state vb -> eval_value_binding ~state vb)
    in
    eval ~state in_
  | Texp_fun (param, body) ->
    let env_addr = Memory.alloc state.memory (Env state.env) ~realm in
    Closure { env = env_addr; param = param.tparam_pat; body }
  | Texp_app (func, arg) ->
    let func_value = eval ~state func in
    let arg_value = eval ~state arg in
    (match func_value with
    | Closure { env; param; body } ->
      (match Memory.deref state.memory env with
      | Env env ->
        let env =
          match match_pattern ~state:{ state with env } arg_value param with
          | None -> raise_s [%message "Interpreter.eval: match error"]
          | Some env -> env
        in
        eval ~state:{ state with env } body
      | _ -> raise_s [%message "Interpreter.eval: expected `Env`"])
    | _ -> raise_s [%message "Interpreter.eval: expected `Closure`"])
  | Texp_tuple exps ->
    let values = List.map exps ~f:(eval ~state) in
    let tuple_addr = Memory.alloc state.memory (Tuple values) ~realm in
    Address tuple_addr
  | Texp_construct (constr, arg) ->
    let arg_value = Option.map arg ~f:(eval ~state) in
    let variant_addr =
      Memory.alloc state.memory (Variant (constr.constructor_name, arg_value)) ~realm
    in
    Address variant_addr
  | Texp_match (matchee, cases) ->
    let matchee_value = eval ~state matchee in
    eval_match ~state matchee_value cases
;;
