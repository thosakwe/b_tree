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
                print_endline value;
                main tree
            | "set" :: key_str :: arg_str ->
                let key = int_of_string key_str in
                let arg = String.concat " " arg_str in
                let new_tree = B_tree.insert tree key arg in
                print_endline ("Inserted " ^ (string_of_int key));
                main new_tree
            | "put" :: arg_str ->
                let key = B_tree.length tree in
                let new_tree = B_tree.insert tree key (String.concat " " arg_str) in
                print_endline ("Inserted " ^ (string_of_int key));
                main new_tree
            | "list" :: _ ->
                let visit (k, v) =
                  print_endline ("Key " ^ (string_of_int k) ^ "=" ^ v)
                in
                B_tree.traverse visit tree;
                main tree
            | ["quit"] -> ()
            | _ -> print_endline "Unknown command."; main tree
  with
    | Not_found -> print_endline "Not_found"; main tree

let () = 
  try main (B_tree.Empty 0) with
    | Sys.Break -> ()
