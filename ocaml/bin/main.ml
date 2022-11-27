open New_wave


let rec repl top_level =
	let s = read_line () |> Lib.Top_level.eval top_level in
	(Printf.sprintf "> %s" s) |> print_string;
	repl top_level

	
let () = repl (Lib.Top_level.create ())
