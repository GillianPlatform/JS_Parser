open Flow_parser.Ast
open Utils

type loc = Flow_parser.Loc.t

type file = Flow_parser.File_key.t

type error = Flow_parser.Parser_common.Error.t

type parse_f = string -> file option -> loc program * (loc * error) list

type transform_f = loc program -> Syntax.exp

module Config = struct
  let runtime_path = "JS_PARSER_RUNTIME_PATH"

  let preamble_file = "preamble.js"
end

let unfold_prog prog =
  let Syntax.{ exp_stx; _ } = prog in
  match exp_stx with
  | Syntax.Script (use_strict, exp_list) -> (use_strict, exp_list)
  | _ -> failwith "expected expression to be a Script"

let get_preamble () =
  let runtime_dir = Sys.getenv Config.runtime_path in
  let preamble_path = Filename.concat runtime_dir Config.preamble_file in
  let prog_string = load_file preamble_path in
  let prog, errors = Flow_parser.Parser_flow.program ~fail:false prog_string in
  let () = check_parsing_errors errors in
  OfFlow.transform_program ~parse_annotations:false ~force_strict:false prog

let file_of_path path : Flow_parser.File_key.t option = Some (SourceFile path)

exception Loader_error

let loader_error path (loc : loc) msg =
  let line_no = loc.start.line in
  Printf.printf "%s: line %d, function 'require':\n%s\n" path line_no msg;
  raise Loader_error

let get_or_raise_exn path loc (result : ('a, string) result) =
  match result with
  | Ok value      -> value
  | Error err_msg -> loader_error path loc err_msg

let resolve_require_args
    (prog_path : string)
    (loc : loc)
    (args : loc Expression.expression_or_spread list) =
  let open Expression in
  let resolve_path path =
    if begins_with path "." || begins_with path ".." then
      (* Local module; treat as relative to dirname of current program *)
      let dirname = Filename.dirname prog_path in
      let path_with_ext =
        if Filename.extension path = "" then path ^ ".js" else path
      in
      let full_path = Filename.concat dirname path_with_ext in
      if Sys.file_exists full_path then Ok (normalize_path full_path)
      else Error (Printf.sprintf "could not resolve module \"%s\"" path)
    else
      (* Global module, e.g. located in node_modules *)
      Error "cannot import global modules"
  in
  let resolve_path_expr = function
    | Literal { value = String path; raw } ->
        let resolved_path =
          get_or_raise_exn prog_path loc (resolve_path path)
        in
        Ok (Literal { value = String resolved_path; raw }, resolved_path)
    | _ -> Error "the 'id' argument must be a string"
  in
  let resolve_args = function
    | []                     -> Error "the 'id' argument must be provided"
    | expr_or_spread :: rest -> (
        match expr_or_spread with
        | Expression (loc, expr) ->
            let resolved_expr, resolved_path =
              get_or_raise_exn prog_path loc (resolve_path_expr expr)
            in
            Ok (Expression (loc, resolved_expr) :: rest, resolved_path)
        | Spread _               -> Error "a spread expression cannot be used" )
  in
  get_or_raise_exn prog_path loc (resolve_args args)

let rec resolve_statement prog_path (statement : loc Statement.t) =
  let open Statement in
  let loc, stat = statement in
  let resolved_stat, req_paths =
    match stat with
    | Block { body } ->
        let body, req_paths = resolve_statements prog_path body in
        (Block { body }, req_paths)
    | FunctionDeclaration func ->
        let func, req_paths = resolve_function prog_path func in
        (FunctionDeclaration func, req_paths)
    | VariableDeclaration var_dec ->
        let var_dec, req_paths = resolve_variable_dec prog_path var_dec in
        (VariableDeclaration var_dec, req_paths)
    | Expression { expression; directive } ->
        let expression, req_paths = resolve_expression prog_path expression in
        (Expression { expression; directive }, req_paths)
    | If { test; consequent; alternate } ->
        let test, test_rps = resolve_expression prog_path test in
        let consequent, con_rps = resolve_statement prog_path consequent in
        let alternate, alt_rps =
          opt_map (resolve_statement prog_path) alternate
        in
        (If { test; consequent; alternate }, test_rps @ con_rps @ alt_rps)
    | Labeled { label; body } ->
        let body, req_paths = resolve_statement prog_path body in
        (Labeled { label; body }, req_paths)
    | With { _object; body } ->
        let _object, _object_rps = resolve_expression prog_path _object in
        let body, body_rps = resolve_statement prog_path body in
        (With { _object; body }, _object_rps @ body_rps)
    | Switch { discriminant; cases } ->
        let discriminant, dis_rps = resolve_expression prog_path discriminant in
        let cases, cases_rps = map (resolve_switch_case prog_path) cases in
        (Switch { discriminant; cases }, dis_rps @ cases_rps)
    | Throw { argument } ->
        let argument, req_paths = resolve_expression prog_path argument in
        (Throw { argument }, req_paths)
    | Try { block; handler; finalizer } ->
        let block, block_rps = resolve_block prog_path block in
        let handler, handler_rps =
          opt_map (resolve_catch_clause prog_path) handler
        in
        let finalizer, fin_rps = opt_map (resolve_block prog_path) finalizer in
        (Try { block; handler; finalizer }, block_rps @ handler_rps @ fin_rps)
    | While { test; body } ->
        let test, test_rps = resolve_expression prog_path test in
        let body, body_rps = resolve_statement prog_path body in
        (While { test; body }, test_rps @ body_rps)
    | DoWhile { body; test } ->
        let body, body_rps = resolve_statement prog_path body in
        let test, test_rps = resolve_expression prog_path test in
        (DoWhile { body; test }, body_rps @ test_rps)
    | For { init; test; update; body } ->
        let init, init_rps = opt_map (resolve_for_init prog_path) init in
        let test, test_rps = opt_map (resolve_expression prog_path) test in
        let update, update_rps =
          opt_map (resolve_expression prog_path) update
        in
        let body, body_rps = resolve_statement prog_path body in
        ( For { init; test; update; body },
          init_rps @ test_rps @ update_rps @ body_rps )
    | ForIn { left; right; body; each } ->
        let left, left_rps = resolve_for_in_left prog_path left in
        let right, right_rps = resolve_expression prog_path right in
        let body, body_rps = resolve_statement prog_path body in
        (ForIn { left; right; body; each }, left_rps @ right_rps @ body_rps)
    | Return { argument } ->
        let argument, req_paths =
          opt_map (resolve_expression prog_path) argument
        in
        (Return { argument }, req_paths)
    | _ -> (stat, [])
  in
  ((loc, resolved_stat), req_paths)

and resolve_expression prog_path (expression : loc Expression.t) =
  let open Expression in
  let loc, expr = expression in
  let resolved_expr, req_paths =
    match expr with
    | Call { callee; targs; arguments } -> (
        match callee with
        | _, Identifier (_, id) when id = SyntaxGenerator.Constants.require_f ->
            let arguments, req_path =
              resolve_require_args prog_path loc arguments
            in
            (Call { callee; targs; arguments }, [ req_path ])
        | _ ->
            let callee, callee_rps = resolve_expression prog_path callee in
            let arguments, arg_rps =
              map (resolve_expression_or_spread prog_path) arguments
            in
            (Call { callee; targs; arguments }, callee_rps @ arg_rps) )
    | New { callee; targs; arguments } ->
        let callee, callee_rps = resolve_expression prog_path callee in
        let arguments, arg_rps =
          map (resolve_expression_or_spread prog_path) arguments
        in
        (New { callee; targs; arguments }, callee_rps @ arg_rps)
    | Unary { operator; prefix; argument } ->
        let argument, req_paths = resolve_expression prog_path argument in
        (Unary { operator; prefix; argument }, req_paths)
    | Binary { operator; left; right } ->
        let left, left_rps = resolve_expression prog_path left in
        let right, right_rps = resolve_expression prog_path right in
        (Binary { operator; left; right }, left_rps @ right_rps)
    | Assignment { operator; left; right } ->
        let left, left_rps = resolve_pattern prog_path left in
        let right, right_rps = resolve_expression prog_path right in
        (Assignment { operator; left; right }, left_rps @ right_rps)
    | Logical { operator; left; right } ->
        let left, left_rps = resolve_expression prog_path left in
        let right, right_rps = resolve_expression prog_path right in
        (Logical { operator; left; right }, left_rps @ right_rps)
    | Update { operator; argument; prefix } ->
        let argument, req_paths = resolve_expression prog_path argument in
        (Update { operator; argument; prefix }, req_paths)
    | Member { _object; property; computed } ->
        let _object, _object_rps = resolve_expression prog_path _object in
        let property, property_rps = resolve_member_prop prog_path property in
        (Member { _object; property; computed }, _object_rps @ property_rps)
    | Object { properties } ->
        let properties, req_paths =
          map (resolve_object_prop prog_path) properties
        in
        (Object { properties }, req_paths)
    | Sequence { expressions } ->
        let expressions, req_paths =
          resolve_expressions prog_path expressions
        in
        (Sequence { expressions }, req_paths)
    | Conditional { test; consequent; alternate } ->
        let test, test_rps = resolve_expression prog_path test in
        let consequent, con_rps = resolve_expression prog_path consequent in
        let alternate, alt_rps = resolve_expression prog_path alternate in
        ( Conditional { test; consequent; alternate },
          test_rps @ con_rps @ alt_rps )
    | Array { elements } ->
        let elements, req_paths =
          map
            (fun elem -> opt_map (resolve_expression_or_spread prog_path) elem)
            elements
        in
        (Array { elements }, req_paths)
    | Function func ->
        let func, req_paths = resolve_function prog_path func in
        (Function func, req_paths)
    | _ -> (expr, [])
  in
  ((loc, resolved_expr), req_paths)

and resolve_expression_or_spread
    prog_path (expr_or_spr : loc Expression.expression_or_spread) =
  match expr_or_spr with
  | Expression expr ->
      let expr, req_paths = resolve_expression prog_path expr in
      (Expression expr, req_paths)
  | Spread spr      -> (Spread spr, [])

and resolve_pattern prog_path (pattern : loc Pattern.t) =
  let open Pattern in
  let loc, ptrn = pattern in
  let resolved_pattern, req_paths =
    match ptrn with
    | Expression expr ->
        let expr, req_paths = resolve_expression prog_path expr in
        (Expression expr, req_paths)
    | _               -> (ptrn, [])
  in
  ((loc, resolved_pattern), req_paths)

and resolve_function prog_path (func : loc Function.t) =
  let open Function in
  let resolve_func_body = function
    | BodyBlock block     ->
        let block, req_paths = resolve_block prog_path block in
        (BodyBlock block, req_paths)
    | BodyExpression expr ->
        let expr, req_paths = resolve_expression prog_path expr in
        (BodyExpression expr, req_paths)
  in
  let body, req_paths = resolve_func_body func.body in
  ({ func with body }, req_paths)

and resolve_variable_dec prog_path (dec : loc Statement.VariableDeclaration.t) =
  let open Statement.VariableDeclaration in
  let resolve_dec dec =
    let loc, Declarator.{ id; init } = dec in
    let init, req_paths = opt_map (resolve_expression prog_path) init in
    ((loc, Declarator.{ id; init }), req_paths)
  in
  let declarations, req_paths = map resolve_dec dec.declarations in
  ({ dec with declarations }, req_paths)

and resolve_for_init prog_path (init : loc Statement.For.init) =
  let open Statement.For in
  let resolve = function
    | InitDeclaration (loc, var_dec) ->
        let var_dec, req_paths = resolve_variable_dec prog_path var_dec in
        (InitDeclaration (loc, var_dec), req_paths)
    | InitExpression expr ->
        let expr, req_paths = resolve_expression prog_path expr in
        (InitExpression expr, req_paths)
  in
  resolve init

and resolve_for_in_left prog_path (left : loc Statement.ForIn.left) =
  let open Statement.ForIn in
  let resolve = function
    | LeftDeclaration (loc, var_dec) ->
        let var_dec, req_paths = resolve_variable_dec prog_path var_dec in
        (LeftDeclaration (loc, var_dec), req_paths)
    | LeftPattern ptrn ->
        let ptrn, req_paths = resolve_pattern prog_path ptrn in
        (LeftPattern ptrn, req_paths)
  in
  resolve left

and resolve_switch_case prog_path (case : loc Statement.Switch.Case.t) =
  let open Statement.Switch.Case in
  let loc, case = case in
  let test, test_rps = opt_map (resolve_expression prog_path) case.test in
  let consequent, con_rps = resolve_statements prog_path case.consequent in
  ((loc, { test; consequent }), test_rps @ con_rps)

and resolve_catch_clause
    prog_path (catch_clause : loc Statement.Try.CatchClause.t) =
  let open Statement.Try.CatchClause in
  let loc, { param; body } = catch_clause in
  let body, req_paths = resolve_block prog_path body in
  ((loc, { param; body }), req_paths)

and resolve_block prog_path (block : loc * loc Statement.Block.t) =
  let open Statement.Block in
  let loc, { body } = block in
  let body, req_paths = resolve_statements prog_path body in
  ((loc, { body }), req_paths)

and resolve_member_prop prog_path (property : loc Expression.Member.property) =
  let open Expression.Member in
  let resolve = function
    | PropertyExpression expr ->
        let expr, req_paths = resolve_expression prog_path expr in
        (PropertyExpression expr, req_paths)
    | other                   -> (other, [])
  in
  resolve property

and resolve_object_prop prog_path (property : loc Expression.Object.property) =
  match property with
  | Property (loc, prop)  ->
      let open Expression.Object.Property in
      let resolved_prop, req_paths =
        match prop with
        | Init { key; value; shorthand } ->
            let value, req_paths = resolve_expression prog_path value in
            (Init { key; value; shorthand }, req_paths)
        | _ -> (prop, [])
      in
      (Property (loc, resolved_prop), req_paths)
  | SpreadProperty spread -> (SpreadProperty spread, [])

and resolve_statements prog_path (statements : loc Statement.t list) =
  map (resolve_statement prog_path) statements

and resolve_expressions prog_path (expressions : loc Expression.t list) =
  map (resolve_expression prog_path) expressions

(** Resolves the paths used in any require() calls within the program, returning
    the modified AST (with all the paths having been checked to exist and 
    normalised) as well as the paths themselves. *)
let resolve_imports prog_path (prog : loc program) =
  let loc, statements, cmnts = prog in
  let resolved_stats, req_paths = resolve_statements prog_path statements in
  ((loc, resolved_stats, cmnts), req_paths)

(** Wraps the module code inside special syntax that hides its variables from
    the global scope and exposes it to the CommonJS [module] and [exports] 
    objects. *)
let augment exp_list use_strict filename is_main : Syntax.exp list =
  let open SyntaxGenerator in
  let dirname = Filename.dirname filename in
  let module_init = module_init_assign filename dirname in
  let cache_init = module_cache_assign filename in
  let module_body =
    [ set_start_load_status ] @ exp_list @ [ set_end_load_satus ]
  in
  let load =
    if is_main then immediate_module_load use_strict module_body
    else module_load_func use_strict module_body
  in
  [ module_init; cache_init; load ]

(** Add the code which, among other things, provides the definiton of the 
    global [require] function. *)
let add_preamble exp_list : Syntax.exp =
  let _, preamble_exp_list = unfold_prog (get_preamble ()) in
  SyntaxGenerator.mk_exp (Script (false, preamble_exp_list @ exp_list))

let parse_commonjs parse transform program_path program_string =
  let open Flow_parser in
  let rec resolve_modules required_paths added_paths combined_prog =
    match required_paths with
    | []           -> combined_prog
    | path :: rest ->
        if not (Str_set.mem path added_paths) then
          let prog, errors = parse (load_file path) (file_of_path path) in
          let () = check_parsing_errors errors in
          let resolved_prog, req_paths = resolve_imports path prog in
          let trans_prog = transform resolved_prog in
          let use_strict, exp_list = unfold_prog trans_prog in
          let augmented_prog = augment exp_list use_strict path false in
          let combined_prog = augmented_prog @ combined_prog in
          let new_required = required_paths @ req_paths in
          let new_added = Str_set.add path added_paths in
          resolve_modules new_required new_added combined_prog
        else resolve_modules rest added_paths combined_prog
  in
  (* Parse and process the main module *)
  let prog, errors = parse program_string (file_of_path program_path) in
  let () = check_parsing_errors errors in
  let prog_path = normalize_path program_path in
  let resolved_prog, req_paths = resolve_imports prog_path prog in
  let trans_prog = transform resolved_prog in
  let use_strict, exp_list = unfold_prog trans_prog in
  let main_prog = augment exp_list use_strict program_path true in
  (* Parse and process any required modules and combine them into one program *)
  add_preamble (resolve_modules req_paths Str_set.empty main_prog)
