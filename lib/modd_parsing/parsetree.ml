open Ast_types

type mode =
  | Pmod_local
  | Pmod_global

type type_ =
  | Ptyp_arrow of type_with_mode * type_with_mode
  | Ptyp_tuple of type_ list
  | Ptyp_constr of type_ list * string

and type_with_mode =
  { ptwm_type : type_
  ; ptwm_mode : mode
  }

type pattern =
  | Ppat_any
  | Ppat_var of string
  | Ppat_const of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of string * pattern option
  | Ppat_constraint of pattern * type_

type function_param =
  { pparam_pat : pattern
  ; pparam_mode : mode
  }

type expression =
  | Pexp_var of string
  | Pexp_const of constant
  | Pexp_uop of unary_op * expression
  | Pexp_bop of binary_op * expression * expression
  | Pexp_let of value_binding list * expression
  | Pexp_fun of function_param * expression
  | Pexp_app of expression * expression
  | Pexp_constraint of expression * type_with_mode
  | Pexp_tuple of expression list
  | Pexp_construct of string * expression option
  | Pexp_match of expression * case list

and value_binding =
  { pvb_pat : pattern
  ; pvb_expr : expression
  }

and case =
  { pc_lhs : pattern
  ; pc_rhs : expression
  }
