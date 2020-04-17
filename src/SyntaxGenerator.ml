open Flow_parser.Ast
module Loc = Flow_parser.Loc

type loc = Loc.t

module Constants = struct
  let module_obj = "Module"

  let module_var = "_module"

  let cache_var = "_cache"

  let require_f = "require"

  let module_params = [ "exports"; "module"; "__filename"; "__dirname" ]

  let not_loaded = "NOT_LOADED"

  let loading = "LOADING"

  let loaded = "LOADED"
end

let rloc : loc =
  (* An arbitrary location, used as a placeholder within nodes *)
  {
    source = None;
    start = { line = -1; column = -1; offset = -1 };
    _end = { line = -1; column = -1; offset = -1 };
  }

let lit_of_string str : Literal.t =
  { value = String str; raw = Printf.sprintf "\"%s\"" str }

let ident_of_string str : loc Identifier.t = (rloc, str)

let expr_of_lit_str str : loc Expression.t =
  (rloc, Expression.Literal (lit_of_string str))

let expr_of_ident_str str : loc Expression.t =
  (rloc, Expression.Identifier (ident_of_string str))

let pattern_of_ident (ident : loc Identifier.t) : loc Pattern.t =
  (rloc, Pattern.Identifier { name = ident; annot = None; optional = false })

let pattern_of_expr expr : loc Pattern.t = (rloc, Pattern.Expression expr)

let expr_or_spr_of_expr expr : loc Expression.expression_or_spread =
  Expression expr

let assign_expr left right : loc Expression.t =
  (rloc, Expression.Assignment { operator = Assign; left; right })

(** Generates: 
    _module = new Module(<filename>, <dirname>); *)
let module_init_stat filename dirname : loc Statement.t =
  let left = pattern_of_ident (ident_of_string Constants.module_var) in
  let callee = expr_of_ident_str Constants.module_obj in
  let arguments =
    List.map
      (fun arg -> expr_or_spr_of_expr (expr_of_lit_str arg))
      [ filename; dirname ]
  in
  let right = (rloc, Expression.New { callee; targs = None; arguments }) in
  let expression = assign_expr left right in
  (rloc, Statement.Expression { expression; directive = None })

(** Generates:
    _cache[<filename>] = _module; *)
let module_cache_stat filename : loc Statement.t =
  let _object = expr_of_ident_str Constants.cache_var in
  let property =
    Expression.Member.PropertyExpression (expr_of_lit_str filename)
  in
  let member_expr =
    (rloc, Expression.Member { _object; property; computed = true })
  in
  let left = pattern_of_expr member_expr in
  let right = expr_of_ident_str Constants.module_var in
  let expression = assign_expr left right in
  (rloc, Statement.Expression { expression; directive = None })

(** Generates:
    module.status = <status>; *)
let module_status_stat status : loc Statement.t =
  let _object = expr_of_ident_str "module" in
  let property =
    Expression.Member.PropertyIdentifier (ident_of_string "status")
  in
  let member_expr =
    (rloc, Expression.Member { _object; property; computed = false })
  in
  let left = pattern_of_expr member_expr in
  let right = expr_of_lit_str status in
  let expression = assign_expr left right in
  (rloc, Statement.Expression { expression; directive = None })

let start_load_stat = module_status_stat Constants.loading

let end_load_stat = module_status_stat Constants.loaded

let func_of_stat_list stat_list param_list : loc Function.t =
  let open Function in
  let params =
    List.map (fun param -> pattern_of_ident (ident_of_string param)) param_list
  in
  let body = BodyBlock (rloc, { body = stat_list }) in
  {
    id = None;
    params = (rloc, { params; rest = None });
    body;
    async = false;
    generator = false;
    predicate = None;
    expression = false;
    return = None;
    tparams = None;
  }

(** Generates the IIFE statement:
    (function (exports, module, __filename, __dirname) {
        ...
        <module_body>
        ...
    })(_module.exports, _module, _module.filename, _module.dirname); *)
let immediate_module_load_stat module_body : loc Statement.t =
  let func = func_of_stat_list module_body Constants.module_params in
  let func_expr = (rloc, Expression.Function func) in
  let arguments =
    List.map
      (fun arg -> expr_or_spr_of_expr (expr_of_ident_str arg))
      [
        Constants.module_var ^ ".exports";
        Constants.module_var;
        Constants.module_var ^ ".filename";
        Constants.module_var ^ ".dirname";
      ]
  in
  let call_expr =
    (rloc, Expression.Call { callee = func_expr; targs = None; arguments })
  in
  (rloc, Statement.Expression { expression = call_expr; directive = None })

(** Generates:
    _module.load = function (exports, module, __filename, __dirname) {
        ...
        <module_body>
        ...
    }; *)
let module_load_stat module_body : loc Statement.t =
  let _object = expr_of_ident_str Constants.module_var in
  let property =
    Expression.Member.PropertyIdentifier (ident_of_string "load")
  in
  let member_expr =
    (rloc, Expression.Member { _object; property; computed = false })
  in
  let left = pattern_of_expr member_expr in
  let func = func_of_stat_list module_body Constants.module_params in
  let func_expr = (rloc, Expression.Function func) in
  let expression = assign_expr left func_expr in
  (rloc, Statement.Expression { expression; directive = None })
