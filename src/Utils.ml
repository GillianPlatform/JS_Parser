open Batteries.Incubator
module Path = PathGen.OfString

let normalize_path path_str =
  let path = Path.of_string path_str in
  let normalized_path = Path.normalize_in_tree path in
  Path.to_string normalized_path

let begins_with str prefix =
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  prefix_len <= str_len && String.sub str 0 prefix_len = prefix

module Str_set = Set.Make (String)

let opt_map f (x : 'a option) =
  match x with
  | None   -> (None, [])
  | Some v ->
      let a, bs = f v in
      (Some a, bs)

let map f (xs : 'a list) =
  List.fold_left
    (fun (acc_a, acc_b) x ->
      let a, bs = f x in
      (a :: acc_a, bs @ acc_b))
    ([], []) xs
