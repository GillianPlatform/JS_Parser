open Flow_parser.Ast

type loc = Flow_parser.Loc.t

let rec resolve_statement (statement : loc Statement.t) =
  let loc, stat = statement in
  let resolved_stat =
    match stat with
    | Statement.Block { body } ->
        Statement.Block { body = resolve_statements body }
    | Statement.FunctionDeclaration func ->
        Statement.FunctionDeclaration (resolve_function func)
    | Statement.VariableDeclaration var_dec ->
        Statement.VariableDeclaration (resolve_variable_dec var_dec)
    | Statement.Expression { expression; directive } ->
        let expression = resolve_expression expression in
        Statement.Expression { expression; directive }
    | Statement.If { test; consequent; alternate } ->
        let test = resolve_expression test in
        let consequent = resolve_statement consequent in
        let alternate = Option.map resolve_statement alternate in
        Statement.If { test; consequent; alternate }
    | Statement.Labeled { label; body } ->
        let body = resolve_statement body in
        Statement.Labeled { label; body }
    | Statement.With { _object; body } ->
        let _object = resolve_expression _object in
        let body = resolve_statement body in
        Statement.With { _object; body }
    | Statement.Switch { discriminant; cases } ->
        let discriminant = resolve_expression discriminant in
        let cases = List.map resolve_switch_case cases in
        Statement.Switch { discriminant; cases }
    | Statement.Throw { argument } ->
        let argument = resolve_expression argument in
        Statement.Throw { argument }
    | Statement.Try { block; handler; finalizer } ->
        let block = resolve_block block in
        let handler = Option.map resolve_catch_clause handler in
        let finalizer = Option.map resolve_block finalizer in
        Statement.Try { block; handler; finalizer }
    | Statement.While { test; body } ->
        let test = resolve_expression test in
        let body = resolve_statement body in
        Statement.While { test; body }
    | Statement.DoWhile { body; test } ->
        let body = resolve_statement body in
        let test = resolve_expression test in
        Statement.DoWhile { body; test }
    | Statement.For { init; test; update; body } ->
        let init = Option.map resolve_for_init init in
        let test = Option.map resolve_expression test in
        let update = Option.map resolve_expression update in
        let body = resolve_statement body in
        Statement.For { init; test; update; body }
    | Statement.ForIn { left; right; body; each } ->
        let left = resolve_for_in_left left in
        let right = resolve_expression right in
        let body = resolve_statement body in
        Statement.ForIn { left; right; body; each }
    | Statement.Return { argument } ->
        let argument = Option.map resolve_expression argument in
        Statement.Return { argument }
    | _ -> stat
  in
  (loc, resolved_stat)

and resolve_expression (expression : loc Expression.t) =
  let loc, expr = expression in
  let resolved_expr =
    match expr with
    | Expression.Call { callee; targs; arguments } ->
        let callee =
          match callee with
          | _, Expression.Identifier (_, id) ->
              print_endline id;
              callee
          | _ -> resolve_expression callee
        in
        let arguments = List.map resolve_expression_or_spread arguments in
        Expression.Call { callee; targs; arguments }
    | Expression.New { callee; targs; arguments } ->
        let callee = resolve_expression callee in
        let arguments = List.map resolve_expression_or_spread arguments in
        Expression.New { callee; targs; arguments }
    | Expression.Unary { operator; prefix; argument } ->
        let argument = resolve_expression argument in
        Expression.Unary { operator; prefix; argument }
    | Expression.Binary { operator; left; right } ->
        let left = resolve_expression left in
        let right = resolve_expression right in
        Expression.Binary { operator; left; right }
    | Expression.Assignment { operator; left; right } ->
        let left = resolve_pattern left in
        let right = resolve_expression right in
        Expression.Assignment { operator; left; right }
    | Expression.Logical { operator; left; right } ->
        let left = resolve_expression left in
        let right = resolve_expression right in
        Expression.Logical { operator; left; right }
    | Expression.Update { operator; argument; prefix } ->
        let argument = resolve_expression argument in
        Expression.Update { operator; argument; prefix }
    | Expression.Member { _object; property; computed } ->
        let _object = resolve_expression _object in
        let property = resolve_member_prop property in
        Expression.Member { _object; property; computed }
    | Expression.Object { properties } ->
        let properties = List.map resolve_object_prop properties in
        Expression.Object { properties }
    | Expression.Sequence { expressions } ->
        Expression.Sequence { expressions = resolve_expressions expressions }
    | Expression.Conditional { test; consequent; alternate } ->
        let test = resolve_expression test in
        let consequent = resolve_expression consequent in
        let alternate = resolve_expression alternate in
        Expression.Conditional { test; consequent; alternate }
    | Expression.Array { elements } ->
        let elements =
          List.map (fun e -> Option.map resolve_expression_or_spread e) elements
        in
        Expression.Array { elements }
    | Expression.Function func -> Expression.Function (resolve_function func)
    | _ -> expr
  in
  (loc, resolved_expr)

and resolve_expression_or_spread
    (expr_or_spr : loc Expression.expression_or_spread) =
  match expr_or_spr with
  | Expression expr -> Expression (resolve_expression expr)
  | Spread spr      -> Spread spr

and resolve_pattern (pattern : loc Pattern.t) =
  let open Pattern in
  let loc, ptrn = pattern in
  let resolved_pattern =
    match ptrn with
    | Expression expr -> Expression (resolve_expression expr)
    | _               -> ptrn
  in
  (loc, resolved_pattern)

and resolve_function (func : loc Function.t) =
  let open Function in
  let resolve_func_body = function
    | BodyBlock block     -> BodyBlock (resolve_block block)
    | BodyExpression expr -> BodyExpression (resolve_expression expr)
  in
  { func with body = resolve_func_body func.body }

and resolve_variable_dec (var_dec : loc Statement.VariableDeclaration.t) =
  let open Statement.VariableDeclaration in
  let resolve_dec dec =
    let loc, Declarator.{ id; init } = dec in
    (loc, Declarator.{ id; init = Option.map resolve_expression init })
  in
  { var_dec with declarations = List.map resolve_dec var_dec.declarations }

and resolve_for_init (init : loc Statement.For.init) =
  let open Statement.For in
  let resolve = function
    | InitDeclaration (loc, var_dec) ->
        InitDeclaration (loc, resolve_variable_dec var_dec)
    | InitExpression expr -> InitExpression (resolve_expression expr)
  in
  resolve init

and resolve_for_in_left (left : loc Statement.ForIn.left) =
  let open Statement.ForIn in
  let resolve = function
    | LeftDeclaration (loc, var_dec) ->
        LeftDeclaration (loc, resolve_variable_dec var_dec)
    | LeftPattern ptrn -> LeftPattern (resolve_pattern ptrn)
  in
  resolve left

and resolve_switch_case (case : loc Statement.Switch.Case.t) =
  let open Statement.Switch.Case in
  let loc, case = case in
  let test = Option.map resolve_expression case.test in
  let consequent = resolve_statements case.consequent in
  (loc, { test; consequent })

and resolve_catch_clause (catch_clause : loc Statement.Try.CatchClause.t) =
  let open Statement.Try.CatchClause in
  let loc, { param; body } = catch_clause in
  (loc, { param; body = resolve_block body })

and resolve_block (block : loc * loc Statement.Block.t) =
  let open Statement.Block in
  let loc, { body } = block in
  (loc, { body = resolve_statements body })

and resolve_member_prop (property : loc Expression.Member.property) =
  let open Expression.Member in
  let resolve = function
    | PropertyExpression exp -> PropertyExpression (resolve_expression exp)
    | other                  -> other
  in
  resolve property

and resolve_object_prop (property : loc Expression.Object.property) =
  match property with
  | Property (loc, prop)  ->
      let open Expression.Object.Property in
      let resolved_prop =
        match prop with
        | Init { key; value; shorthand } ->
            let value = resolve_expression value in
            Init { key; value; shorthand }
        | _ -> prop
      in
      Property (loc, resolved_prop)
  | SpreadProperty spread -> SpreadProperty spread

and resolve_statements (statements : loc Statement.t list) =
  List.map resolve_statement statements

and resolve_expressions (expressions : loc Expression.t list) =
  List.map resolve_expression expressions

let preprocess_as_module program_path prog =
  print_endline program_path;
  let _, stat_list, _ = prog in
  let _ = resolve_statements stat_list in
  prog
