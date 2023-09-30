open! Core

module Type_var : sig
  type t = string

  include Comparable.S with type t := t
end =
  String

type mode =
  | Tmod_local
  | Tmod_global

type type_expr =
  | Ttyp_arrow of type_expr_with_mode * type_expr_with_mode
  | Ttyp_tuple of type_expr list
  | Ttyp_constr of type_constr

and type_var = Type_var.t
and type_constr = type_expr list * string

and type_expr_with_mode =
  { ttwm_type : type_expr
  ; ttwm_mode : mode
  }

type type_declaration =
  { type_name : string
  ; type_kind : type_decl_kind
  }

and type_decl_kind =
  | Type_variant of constructor_declaration list
  | Type_abstract

and constructor_declaration =
  { constructor_name : string
  ; constructor_alphas : type_var list
  ; constructor_arg : type_expr option
  ; constructor_type : type_expr
  }

(* Constructor and record label descriptions *)

type constructor_description =
  { constructor_name : string
  ; constructor_arg : type_expr option
  ; constructor_type : type_expr
  }
