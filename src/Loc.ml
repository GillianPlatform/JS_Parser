type position = Flow_parser.Loc.position = { line : int; column : int }

type t = Flow_parser.Loc.t = {
  source : Flow_parser.File_key.t option;
  start : position;
  _end : position;
}

let pp : Format.formatter -> t -> unit = Flow_parser.Loc.pp
