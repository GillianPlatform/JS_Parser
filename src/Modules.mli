open Flow_parser.Ast

type loc = Flow_parser.Loc.t

type file = Flow_parser.File_key.t

type error = Flow_parser.Parser_common.Error.t

type parse_f = string -> file option -> loc program * (loc * error) list

type transform_f = loc program -> Syntax.exp

val parse_commonjs : parse_f -> transform_f -> string -> string -> Syntax.exp
