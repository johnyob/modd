open! Core

type constant =
  | Const_unit
  | Const_int of int
[@@deriving equal]

type unary_op =
  | Uop_ref
  | Uop_deref
[@@deriving equal]

type binary_op =
  | Bop_add
  | Bop_sub
  | Bop_mul
  | Bop_assign
[@@deriving equal]
