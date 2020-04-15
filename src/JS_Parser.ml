module Syntax = Syntax
module PrettyPrint = PrettyPrint
module Error = Error

let search_forward_safe r s : int option =
  try Some (Str.search_forward r s 0) with Not_found -> None

let parse_string_exn
    ?(parse_annotations = true) ?(force_strict = false) ?program_path prog =
  let parse_options =
    Some
      Flow_parser.Parser_env.
        { default_parse_options with types = false; use_strict = force_strict }
  in
  let flow_prog, errors =
    Flow_parser.Parser_flow.program_file ~fail:false ~parse_options prog None
  in
  if List.length errors > 0 then
    let pretty_messages =
      List.map
        (fun (loc, err) ->
          Flow_parser.Loc.to_string loc
          ^ " : "
          ^ Flow_parser.Parse_error.PP.error err)
        errors
    in
    let messages = String.concat "\n" pretty_messages in
    let error_type =
      match
        search_forward_safe
          (Str.regexp "Invalid left-hand side in assignment")
          (List.hd pretty_messages)
      with
      | Some _ -> "ReferenceError"
      | None   -> "SyntaxError"
    in
    raise (Error.ParserError (Error.FlowParser (messages, error_type)))
  else
    let flow_prog =
      if Option.is_none program_path then flow_prog
      else Modules.preprocess_as_module (Option.get program_path) flow_prog
    in
    let trans_prog =
      OfFlow.transform_program ~parse_annotations ~parent_strict:force_strict
        flow_prog
    in
    trans_prog

let parse_string
    ?(parse_annotations = true) ?(force_strict = false) ?program_path program =
  try
    Ok (parse_string_exn ~parse_annotations ~force_strict ?program_path program)
  with Error.ParserError err -> Error err
