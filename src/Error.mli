type t =
  | Overlapping_Syntax
      (** Something went wrong with the parser, some syntax is overlapping. *)
  | Unhandled_Statement  of int
      (** The statement at the given offset is not handled. Maybe because it 
          is not part of ES5. *)
  | Unhandled_Expression of int
      (** The expression at the given offset is not handled. Maybe because it 
          is not part of ES5. *)
  | NotEcmaScript5       of string * int
      (** Something used in the script is not part of ES5. *)
  | UnusedAnnotations    of string list * int
      (** Some JS_Logic annotations were in the wrong place. *)
  | FlowParser           of string * string
      (** Some error happened at the [flow_parser] level. *)
  | LoaderError          of string * int * string * string
      (** Some error happened when trying to process CommonJS constructs such
          as [require]. *)

val str : t -> string

exception ParserError of t
