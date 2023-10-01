open! Import
open! Types

type realm =
  | Stack
  | Heap
[@@deriving equal, sexp]

type address =
  { addr_realm : realm
  ; addr : int
  }
[@@deriving equal, sexp]

type value =
  | Abstraction of
      { env : address
      ; type_vars : type_var list
      ; body : Typedtree.expression
      }
  | Closure of
      { env : address
      ; param : Typedtree.pattern
      ; body : Typedtree.expression
      }
  | Constant of constant
  | Address of address
[@@deriving sexp]

module Env = struct
  type t =
    { values : value String.Map.t
    ; types : Types.type_expr Type_var.Map.t
    ; layer : layer
    }

  and layer =
    | Region of
        { start : int
        ; next : t
        }
    | Nothing
  [@@deriving sexp]

  let empty = { values = String.Map.empty; types = Type_var.Map.empty; layer = Nothing }
  let add t ~var ~value = { t with values = Map.set t.values ~key:var ~data:value }

  let enter_region t ~start =
    { values = String.Map.empty
    ; types = Type_var.Map.empty
    ; layer = Region { start; next = t }
    }
  ;;

  let exit_region t ~epilogue =
    match t.layer with
    | Nothing -> raise_s [%message "Env.exit_region: expected to be in a region"]
    | Region { start; next = _ } -> epilogue start
  ;;

  let add_type t ~type_var ~type_expr =
    { t with types = Map.set t.types ~key:type_var ~data:type_expr }
  ;;

  let find t var =
    try Map.find_exn t.values var with
    | _ -> raise_s [%message "Unbound variable"]
  ;;

  let find_type t tvar =
    try Map.find_exn t.types tvar with
    | _ -> raise_s [%message "Unbound type variable"]
  ;;
end

module Heap = struct
  type 'a t = { memory : 'a Dynarray.t } [@@deriving sexp]

  let create () = { memory = Dynarray.create () }
  let deref t addr = Dynarray.get t.memory addr
  let assign t addr x = Dynarray.set t.memory addr x

  let alloc t x =
    Dynarray.add_last t.memory x;
    Dynarray.length t.memory - 1
  ;;
end

module Memory = struct
  type block =
    | Env of Env.t
    | Tuple of value list
    | Variant of string * value option
    | Ref of value

  and t =
    { stack : block Dynarray.t
    ; heap : block Heap.t
    }
  [@@deriving sexp]

  let create () = { stack = Dynarray.create (); heap = Heap.create () }

  let deref t ({ addr; addr_realm } : address) =
    match addr_realm with
    | Stack -> Dynarray.get t.stack addr
    | Heap -> Heap.deref t.heap addr
  ;;

  let assign t ({ addr; addr_realm } : address) x =
    match addr_realm with
    | Stack -> Dynarray.set t.stack addr x
    | Heap -> Heap.assign t.heap addr x
  ;;

  let alloc t x ~realm:addr_realm =
    match addr_realm with
    | Stack ->
      let addr = { addr = Dynarray.length t.stack; addr_realm } in
      Dynarray.add_last t.stack x;
      addr
    | Heap ->
      let addr = Heap.alloc t.heap x in
      { addr; addr_realm }
  ;;
end
