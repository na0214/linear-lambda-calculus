let () = print_string (string_of_bool (Type.check_qual_contain_context Type.Un [("a",Type.Bool Type.Lin);("b",Type.Bool Type.Un)]) ^ "\n");;
