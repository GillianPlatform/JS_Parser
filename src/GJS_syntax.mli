exception CannotHappen

type comparison_op =
  | Equal
  | NotEqual
  | TripleEqual
  | NotTripleEqual
  | Lt
  | Le
  | Gt
  | Ge
  | In
  | InstanceOf

type arith_op =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Ursh
  | Lsh
  | Rsh
  | Bitand
  | Bitor
  | Bitxor

type bool_op = And | Or

type bin_op =
  | Comparison of comparison_op
  | Arith      of arith_op
  | Boolean    of bool_op

type unary_op =
  | Not
  | TypeOf
  | Positive
  | Negative
  | Pre_Decr
  | Post_Decr
  | Pre_Incr
  | Post_Incr
  | Bitnot
  | Void

type var = string

type annotation_type =
  | TopRequires
  | TopEnsures
  | TopEnsuresErr
  | Requires
  | Ensures
  | EnsuresErr
  | Id
  | Codename
  | Pred
  | OnlySpec
  | Invariant
  | Lemma
  | Tactic
  | BiAbduce
  | Call
  | JSIL_only

type annotation = { annot_type : annotation_type; annot_formula : string }

type propname =
  | PropnameId     of string
  | PropnameString of string
  | PropnameNum    of float

type proptype = PropbodyVal | PropbodyGet | PropbodySet

type exp = {
  exp_loc : Loc.t;
  exp_stx : exp_syntax;
  exp_annot : annotation list;
}

and exp_syntax =
  | Num           of float
  | String        of string
  | Label         of string * exp
  | Null
  | Bool          of bool
  | Var           of var
  | If            of exp * exp * exp option
  | While         of exp * exp
  | DoWhile       of exp * exp
  | VarDec        of (var * exp option) list
  | This
  | Delete        of exp
  | Comma         of exp * exp
  | Unary_op      of unary_op * exp
  | BinOp         of exp * bin_op * exp
  | Access        of exp * string
  | Call          of exp * exp list
  | Assign        of exp * exp
  | AssignOp      of exp * arith_op * exp
  | FunctionExp   of bool * string option * var list * exp
  | Function      of bool * string option * var list * exp
  | New           of exp * exp list
  | Obj           of (propname * proptype * exp) list
  | Array         of exp option list
  | CAccess       of exp * exp
  | With          of exp * exp
  | Skip
  | Throw         of exp
  | Return        of exp option
  | RegExp        of string * string
  | For           of exp option * exp option * exp option * exp
  | ForIn         of exp * exp * exp
  | Break         of string option
  | Continue      of string option
  | Try           of exp * (string * exp) option * exp option
  | Switch        of exp * (switch_case * exp) list
  | Debugger
  | ConditionalOp of exp * exp * exp
  | Block         of exp list
  | Script        of bool * exp list

and switch_case = Case of exp | DefaultCase

val mk_exp : exp_syntax -> Loc.t -> annotation list -> exp

val script_and_strict : exp_syntax -> bool
