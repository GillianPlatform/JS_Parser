module Syntax : sig
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
end

module PrettyPrint : module type of struct
  include PrettyPrint
end

module Loc : sig
  type file_key

  type position = { line : int; column : int }

  type t = { source : file_key option; start : position; _end : position }

  val file_key_to_string : file_key -> string

  val pp : Format.formatter -> t -> unit
end

module Error : sig
  type t =
    | Overlapping_Syntax
        (** Something went wrong with the parser, some syntax is overlapping. *)
    | Unhandled_Statement  of Loc.t
        (** The statement at the given offset is not handled. Maybe because it 
            is not part of ES5. *)
    | Unhandled_Expression of Loc.t
        (** The expression at the given offset is not handled. Maybe because it 
            is not part of ES5. *)
    | NotEcmaScript5       of string * Loc.t
        (** Something used in the script is not part of ES5. *)
    | UnusedAnnotations    of string list * Loc.t
        (** Some JS_Logic annotations were in the wrong place. *)
    | FlowParser           of string * string
        (** Some error happened at the [flow_parser] level. *)
    | LoaderError          of string * int * string * string
        (** Some error happened when trying to process CommonJS constructs such
            as [require]. *)

  val str : t -> string

  exception ParserError of t
end

val parse_string_exn :
  ?parse_annotations:bool ->
  ?force_strict:bool ->
  ?program_path:string ->
  string ->
  Syntax.exp
(** [parse_string_exn ~parse_annotations ~force_strict prog] parses the given 
    string as a program. The string given should be the entire program. If 
    [parse_annotations] is set to [false], any possible JS_Logic annotations
    in the comments will not be parsed. It is [true] by default. If 
    [force_strict] is true, the program has to be strict. It is [false] by
    default. If there is an error during the parsing, an exception of type 
    {!Error.ParserError} is raised. *)

val parse_string :
  ?parse_annotations:bool ->
  ?force_strict:bool ->
  ?program_path:string ->
  string ->
  (Syntax.exp, Error.t) result
(** Same as [parse_string_exn] except that it returns a result instead of 
    raising an error. *)
