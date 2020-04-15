open Flow_parser.Ast

type loc = Flow_parser.Loc.t

val preprocess_as_module : string -> loc program -> loc program
