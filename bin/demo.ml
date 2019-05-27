let rec main tree =
  try
    match LNoise.linenoise "b_tree> " with
      | None -> main tree
      | Some line ->
          ignore (LNoise.history_add line);
          match  Str.split (Str.regexp " ") line with
            | "get" :: key_str :: _ ->
                let key = int_of_string key_str in
                let value = B_tree.search tree key in
                print_endline (string_of_float value);
                main tree
            | "set" :: key_str :: arg_str :: _ ->
                let key = int_of_string key_str in
                let arg = float_of_string arg_str in
                let new_tree = B_tree.insert tree key arg in
                main new_tree
            | ["quit"] -> ()
            | _ -> print_endline "Unknown command."; main tree
  with
    | Not_found -> print_endline "Not_found"; main tree

let () = 
  try main (B_tree.Empty 0) with
    | Sys.Break -> ()
