open! Import
open Ast_types
open Types

type 'a instance = 'a * type_expr list
and 'a abstraction = type_var list * 'a

type pattern =
  { pat_desc : pattern_desc
  ; pat_type : type_expr
  }

and pattern_desc =
  | Tpat_any
  | Tpat_var of string
  | Tpat_const of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern option

type function_param =
  { tparam_pat : pattern
  ; tparam_mode : mode
  }

type expression =
  { exp_desc : expression_desc
  ; exp_type : type_expr
  ; exp_mode : mode
  }

and expression_desc =
  | Texp_var of string instance
  | Texp_const of constant
  | Texp_uop of unary_op * expression
  | Texp_bop of binary_op * expression * expression
  | Texp_let of value_binding list * expression
  | Texp_fun of function_param * expression
  | Texp_app of expression * expression
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression option
  | Texp_match of expression * case list

and value_binding =
  { tvb_pat : pattern
  ; tvb_expr : expression abstraction
  }

and case =
  { tc_lhs : pattern
  ; tc_rhs : expression
  }
