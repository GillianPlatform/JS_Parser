module Syntax : module type of struct
  include GJS_syntax
end

module PrettyPrint : module type of struct
  include PrettyPrint
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
