open Flow_parser.Ast
module Loc = Flow_parser.Loc

type loc = Loc.t

module Constants = struct
  let require_f = "require"
end

exception Loader_error

let loader_error path (loc : loc) msg =
  let line_no = loc.start.line in
  Printf.printf "%s: line %d, function 'require':\n%s\n" path line_no msg;
  raise Loader_error

let get_or_raise_exn path loc (result : ('a, string) result) =
  match result with
  | Ok value      -> value
  | Error err_msg -> loader_error path loc err_msg

let begins_with str prefix =
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  prefix_len <= str_len && String.sub str 0 prefix_len = prefix

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
      if Sys.file_exists full_path then Ok full_path
      else Error (Printf.sprintf "could not resolve module '%s'" path)
    else
      (* Global module, e.g. located in node_modules *)
      Error "cannot import global modules"
  in
  let resolve_path_expr = function
    | Literal { value = String path; raw } ->
        let resolved_path =
          get_or_raise_exn prog_path loc (resolve_path path)
        in
        Ok (Literal { value = String resolved_path; raw })
    | _ -> Error "the 'id' argument must be a string"
  in
  let resolve_args = function
    | []                     -> Error "the 'id' argument must be provided"
    | expr_or_spread :: rest -> (
        match expr_or_spread with
        | Expression (loc, expr) ->
            let resolved_expr =
              get_or_raise_exn prog_path loc (resolve_path_expr expr)
            in
            Ok (Expression (loc, resolved_expr) :: rest)
        | Spread _               -> Error "a spread expression cannot be used" )
  in
  get_or_raise_exn prog_path loc (resolve_args args)

let rec resolve_statement prog_path (statement : loc Statement.t) =
  let loc, stat = statement in
  let resolved_stat =
    match stat with
    | Statement.Block { body } ->
        Statement.Block { body = resolve_statements prog_path body }
    | Statement.FunctionDeclaration func ->
        Statement.FunctionDeclaration (resolve_function prog_path func)
    | Statement.VariableDeclaration var_dec ->
        Statement.VariableDeclaration (resolve_variable_dec prog_path var_dec)
    | Statement.Expression { expression; directive } ->
        let expression = resolve_expression prog_path expression in
        Statement.Expression { expression; directive }
    | Statement.If { test; consequent; alternate } ->
        let test = resolve_expression prog_path test in
        let consequent = resolve_statement prog_path consequent in
        let alternate = Option.map (resolve_statement prog_path) alternate in
        Statement.If { test; consequent; alternate }
    | Statement.Labeled { label; body } ->
        let body = resolve_statement prog_path body in
        Statement.Labeled { label; body }
    | Statement.With { _object; body } ->
        let _object = resolve_expression prog_path _object in
        let body = resolve_statement prog_path body in
        Statement.With { _object; body }
    | Statement.Switch { discriminant; cases } ->
        let discriminant = resolve_expression prog_path discriminant in
        let cases = List.map (resolve_switch_case prog_path) cases in
        Statement.Switch { discriminant; cases }
    | Statement.Throw { argument } ->
        let argument = resolve_expression prog_path argument in
        Statement.Throw { argument }
    | Statement.Try { block; handler; finalizer } ->
        let block = resolve_block prog_path block in
        let handler = Option.map (resolve_catch_clause prog_path) handler in
        let finalizer = Option.map (resolve_block prog_path) finalizer in
        Statement.Try { block; handler; finalizer }
    | Statement.While { test; body } ->
        let test = resolve_expression prog_path test in
        let body = resolve_statement prog_path body in
        Statement.While { test; body }
    | Statement.DoWhile { body; test } ->
        let body = resolve_statement prog_path body in
        let test = resolve_expression prog_path test in
        Statement.DoWhile { body; test }
    | Statement.For { init; test; update; body } ->
        let init = Option.map (resolve_for_init prog_path) init in
        let test = Option.map (resolve_expression prog_path) test in
        let update = Option.map (resolve_expression prog_path) update in
        let body = resolve_statement prog_path body in
        Statement.For { init; test; update; body }
    | Statement.ForIn { left; right; body; each } ->
        let left = resolve_for_in_left prog_path left in
        let right = resolve_expression prog_path right in
        let body = resolve_statement prog_path body in
        Statement.ForIn { left; right; body; each }
    | Statement.Return { argument } ->
        let argument = Option.map (resolve_expression prog_path) argument in
        Statement.Return { argument }
    | _ -> stat
  in
  (loc, resolved_stat)

and resolve_expression prog_path (expression : loc Expression.t) =
  let loc, expr = expression in
  let resolved_expr =
    match expr with
    | Expression.Call { callee; targs; arguments } -> (
        match callee with
        | _, Expression.Identifier (_, id) when id = Constants.require_f ->
            let arguments = resolve_require_args prog_path loc arguments in
            Expression.Call { callee; targs; arguments }
        | _ ->
            let callee = resolve_expression prog_path callee in
            let arguments =
              List.map (resolve_expression_or_spread prog_path) arguments
            in
            Expression.Call { callee; targs; arguments } )
    | Expression.New { callee; targs; arguments } ->
        let callee = resolve_expression prog_path callee in
        let arguments =
          List.map (resolve_expression_or_spread prog_path) arguments
        in
        Expression.New { callee; targs; arguments }
    | Expression.Unary { operator; prefix; argument } ->
        let argument = resolve_expression prog_path argument in
        Expression.Unary { operator; prefix; argument }
    | Expression.Binary { operator; left; right } ->
        let left = resolve_expression prog_path left in
        let right = resolve_expression prog_path right in
        Expression.Binary { operator; left; right }
    | Expression.Assignment { operator; left; right } ->
        let left = resolve_pattern prog_path left in
        let right = resolve_expression prog_path right in
        Expression.Assignment { operator; left; right }
    | Expression.Logical { operator; left; right } ->
        let left = resolve_expression prog_path left in
        let right = resolve_expression prog_path right in
        Expression.Logical { operator; left; right }
    | Expression.Update { operator; argument; prefix } ->
        let argument = resolve_expression prog_path argument in
        Expression.Update { operator; argument; prefix }
    | Expression.Member { _object; property; computed } ->
        let _object = resolve_expression prog_path _object in
        let property = resolve_member_prop prog_path property in
        Expression.Member { _object; property; computed }
    | Expression.Object { properties } ->
        let properties = List.map (resolve_object_prop prog_path) properties in
        Expression.Object { properties }
    | Expression.Sequence { expressions } ->
        let expressions = resolve_expressions prog_path expressions in
        Expression.Sequence { expressions }
    | Expression.Conditional { test; consequent; alternate } ->
        let test = resolve_expression prog_path test in
        let consequent = resolve_expression prog_path consequent in
        let alternate = resolve_expression prog_path alternate in
        Expression.Conditional { test; consequent; alternate }
    | Expression.Array { elements } ->
        let elements =
          List.map
            (fun elem ->
              Option.map (resolve_expression_or_spread prog_path) elem)
            elements
        in
        Expression.Array { elements }
    | Expression.Function func ->
        Expression.Function (resolve_function prog_path func)
    | _ -> expr
  in
  (loc, resolved_expr)

and resolve_expression_or_spread
    prog_path (expr_or_spr : loc Expression.expression_or_spread) =
  match expr_or_spr with
  | Expression expr -> Expression (resolve_expression prog_path expr)
  | Spread spr      -> Spread spr

and resolve_pattern prog_path (pattern : loc Pattern.t) =
  let open Pattern in
  let loc, ptrn = pattern in
  let resolved_pattern =
    match ptrn with
    | Expression expr -> Expression (resolve_expression prog_path expr)
    | _               -> ptrn
  in
  (loc, resolved_pattern)

and resolve_function prog_path (func : loc Function.t) =
  let open Function in
  let resolve_func_body = function
    | BodyBlock block     -> BodyBlock (resolve_block prog_path block)
    | BodyExpression expr -> BodyExpression (resolve_expression prog_path expr)
  in
  { func with body = resolve_func_body func.body }

and resolve_variable_dec
    prog_path (var_dec : loc Statement.VariableDeclaration.t) =
  let open Statement.VariableDeclaration in
  let resolve_dec dec =
    let loc, Declarator.{ id; init } = dec in
    let init = Option.map (resolve_expression prog_path) init in
    (loc, Declarator.{ id; init })
  in
  { var_dec with declarations = List.map resolve_dec var_dec.declarations }

and resolve_for_init prog_path (init : loc Statement.For.init) =
  let open Statement.For in
  let resolve = function
    | InitDeclaration (loc, var_dec) ->
        InitDeclaration (loc, resolve_variable_dec prog_path var_dec)
    | InitExpression expr -> InitExpression (resolve_expression prog_path expr)
  in
  resolve init

and resolve_for_in_left prog_path (left : loc Statement.ForIn.left) =
  let open Statement.ForIn in
  let resolve = function
    | LeftDeclaration (loc, var_dec) ->
        LeftDeclaration (loc, resolve_variable_dec prog_path var_dec)
    | LeftPattern ptrn -> LeftPattern (resolve_pattern prog_path ptrn)
  in
  resolve left

and resolve_switch_case prog_path (case : loc Statement.Switch.Case.t) =
  let open Statement.Switch.Case in
  let loc, case = case in
  let test = Option.map (resolve_expression prog_path) case.test in
  let consequent = resolve_statements prog_path case.consequent in
  (loc, { test; consequent })

and resolve_catch_clause
    prog_path (catch_clause : loc Statement.Try.CatchClause.t) =
  let open Statement.Try.CatchClause in
  let loc, { param; body } = catch_clause in
  (loc, { param; body = resolve_block prog_path body })

and resolve_block prog_path (block : loc * loc Statement.Block.t) =
  let open Statement.Block in
  let loc, { body } = block in
  (loc, { body = resolve_statements prog_path body })

and resolve_member_prop prog_path (property : loc Expression.Member.property) =
  let open Expression.Member in
  let resolve = function
    | PropertyExpression exp ->
        PropertyExpression (resolve_expression prog_path exp)
    | other                  -> other
  in
  resolve property

and resolve_object_prop prog_path (property : loc Expression.Object.property) =
  match property with
  | Property (loc, prop)  ->
      let open Expression.Object.Property in
      let resolved_prop =
        match prop with
        | Init { key; value; shorthand } ->
            let value = resolve_expression prog_path value in
            Init { key; value; shorthand }
        | _ -> prop
      in
      Property (loc, resolved_prop)
  | SpreadProperty spread -> SpreadProperty spread

and resolve_statements prog_path (statements : loc Statement.t list) =
  List.map (resolve_statement prog_path) statements

and resolve_expressions prog_path (expressions : loc Expression.t list) =
  List.map (resolve_expression prog_path) expressions

let preprocess_as_module program_path prog =
  let loc, statements, comments = prog in
  (* Resolve any require() calls *)
  let resolved_stats = resolve_statements program_path statements in
  (loc, resolved_stats, comments)
