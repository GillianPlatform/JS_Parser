open Flow_parser.Ast
module Loc = Flow_parser.Loc

type loc = Loc.t

val preprocess_as_module : string -> loc program -> loc program
