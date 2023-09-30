// end of file
%token EOF

// keywords
%token FUN
%token LOCAL
%token GLOBAL
%token LET
%token IN
%token MATCH
%token WITH
%token REF
%token AND

// operators
%token RIGHT_ARROW
%token EQUAL
%token COLON
%token AT
%token PLUS
%token MINUS
%token STAR
%token MARK
%token WALRUS
%token BAR
%token UNDERSCORE
%token COMMA

// literals
%token UNIT
%token <int> INT

// identifiers
%token <string> IDENT
%token <string> UIDENT

// parens
%token LEFT_PAREN
%token RIGHT_PAREN

// prcedence
%right RIGHT_ARROW
%right WALRUS
%left PLUS MINUS
%left STAR
%nonassoc prec_unary_op

%{
open Ast_types
open Parsetree

let type_with_mode ?(mode = Pmod_global) type_ =
  { ptwm_type = type_; ptwm_mode = mode }
;;

let unary_op ~op ~exp = 
  Pexp_uop (op, exp)
;;

let binary_op ~op ~exp1 ~exp2 = 
  Pexp_bop (op, exp1, exp2)
;;
%}

%start parse_expression
%type <expression> parse_expression

%type <type_> type_
%type <constant> constant
%type <expression> expression

%%

// Generic rules

separated_nontrivial_list(sep, X):
  | x1 = X
    ; sep
    ; x2 = X
      { [ x1; x2 ] }
  | x = X
    ; sep
    ; xs = separated_nontrivial_list(sep, X)
      { x :: xs }

// Parsing Modd

parse_expression:
  exp = expression 
  ; EOF 
      { exp }

constant:
  | UNIT            
      { Const_unit }
  | int = INT
      { Const_int int }

mode:
  | GLOBAL
      { Pmod_global }
  | LOCAL
      { Pmod_local }

type_:
  | type_ = arrow_type
      { type_ }

type_with_mode(T):
  | type_ = T
    ; mode = option(AT; mode = mode { mode })
      { type_with_mode ?mode type_ }

arrow_type:
  | type_ = tuple_type
      { type_ }   
  | twm1 = type_with_mode(arrow_type)
    ; RIGHT_ARROW
    ; twm2 = type_with_mode(arrow_type)
      { Ptyp_arrow (twm1, twm2) }

tuple_type:
  | type_ = atom_type
      { type_ }
  | types = separated_nontrivial_list(STAR, atom_type)
      { Ptyp_tuple types }

atom_type:
  | LEFT_PAREN
    ; type_ = type_
    ; RIGHT_PAREN
      { type_ }
  | types = type_argument_list
    ; id = IDENT
      { Ptyp_constr (types, id) }

%inline type_argument_list:
  | /* empty */   
      { [] }
  | type_ = atom_type 
      { [ type_ ] }
  | LEFT_PAREN
    ; types = separated_nontrivial_list(COMMA, type_)
    ; RIGHT_PAREN
      { types }

pattern:
  | pat = construct_pattern
      { pat }

construct_pattern:
  | pat = atom_pattern
      { pat }
  | con_id = UIDENT
    ; con_pat_arg = pattern
      { Ppat_construct (con_id, Some con_pat_arg) }

atom_pattern:
  | const = constant
      { Ppat_const const }
  | UNDERSCORE      
      { Ppat_any }
  | id = IDENT            
      { Ppat_var id }
  | con_id = UIDENT
      { Ppat_construct (con_id, None) }
  | LEFT_PAREN 
    ; pats = separated_nontrivial_list(COMMA, pattern)
    ; RIGHT_PAREN
      { Ppat_tuple pats }
  | LEFT_PAREN
    ; pat = pattern
    ; COLON
    ; type_ = type_
    ; RIGHT_PAREN
      { Ppat_constraint (pat, type_) }
  | LEFT_PAREN
    ; pat = pattern
    ; RIGHT_PAREN
      { pat }  

function_param:
  | pat = pattern
      { { pparam_pat = pat; pparam_mode = Pmod_global } }
  | LEFT_PAREN
    ; mode = mode
    ; pat = pattern
    ; RIGHT_PAREN
      { { pparam_pat = pat; pparam_mode = mode } }

%inline unary_op:
  | MARK        { Uop_deref }
  | REF         { Uop_ref }

%inline binary_op:
  | WALRUS      { Bop_assign }
  | PLUS        { Bop_add }
  | MINUS       { Bop_sub }
  | STAR        { Bop_mul }

expression:
  | exp = app_expression                                                                   
      { exp }
  | op = unary_op
    ; exp = expression %prec prec_unary_op
      { unary_op ~op ~exp }
  | exp1 = expression
    ; op = binary_op
    ; exp2 = expression 
      { binary_op ~op ~exp1 ~exp2 }
  | FUN
    ; param = function_param
    ; RIGHT_ARROW
    ; exp = expression 
      { Pexp_fun (param, exp) }
  | MATCH
    ; exp = expression
    ; WITH
    ; cases = cases
      { Pexp_match (exp, cases) }
  | LET
    ; value_bindings = value_bindings
    ; IN
    ; exp = expression
      { Pexp_let (value_bindings, exp) }

app_expression:
  | exp = atom_expression
      { exp }
  | exp1 = app_expression; exp2 = atom_expression
      { match exp1 with
        | Pexp_construct (con_id, None) -> Pexp_construct (con_id, Some exp2)
        | _ -> Pexp_app (exp1, exp2)
      }

%inline value_bindings:
  value_bindings = separated_nonempty_list(AND, value_binding)
    { value_bindings }

value_binding:
  pat = pattern 
  ; EQUAL
  ; exp = expression
    { {pvb_pat = pat; pvb_expr = exp } }

%inline cases:
  LEFT_PAREN
  ; cases = separated_nonempty_list(BAR, case)
  ; RIGHT_PAREN
    { cases }

case:
  pat = pattern
  ; RIGHT_ARROW
  ; exp = expression
      { { pc_lhs = pat; pc_rhs = exp } }

atom_expression:
  | const = constant 
      { Pexp_const const }
  | id = IDENT
      { Pexp_var id }
  | LEFT_PAREN
    ; exp = expression
    ; RIGHT_PAREN
      { exp }
  | LEFT_PAREN
    ; exp = expression
    ; COLON
    ; twm = type_with_mode(type_)
    ; RIGHT_PAREN
      { Pexp_constraint (exp, twm) }

