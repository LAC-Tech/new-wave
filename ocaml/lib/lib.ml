open Base

module Top_level = struct
	type t = { ds: int Stack.t }

	let create () = { ds = Stack.create () }

	let tokenize = 
		(let by_whitespace = (Str.regexp "[ \n\r\x0c\t]+") in
		Str.split by_whitespace)

	let exec_token (top_level: t) (token: string) =
		let bin_op op = match (Stack.pop top_level.ds, Stack.pop top_level.ds) with
			| (Some x, Some y) -> Ok(op y x |> Stack.push top_level.ds)
			| _ -> Error("stack underflow")
		in
		match token with
			| "+" -> bin_op (+)
			| "-" -> bin_op (-)
			| "/" -> bin_op (/)
			| t -> (
				match Caml.int_of_string_opt token with 
				| Some n -> Ok(Stack.push top_level.ds n)
				| None -> Error (Printf.sprintf "%s is not a number" t)
			)

	let to_string top_level = 
		if Stack.is_empty top_level.ds then
			""
		else
			let values = top_level.ds
				|> Stack.to_list
				|> List.map ~f:Int.to_string 
				|> List.rev 
				|> String.concat ~sep:" "
			in
			let types = List.init (Stack.length top_level.ds) ~f: (Fn.const "num")
				|> String.concat ~sep:" "
			in Printf.sprintf "%s : %s\n" values types

	let rec exec_tokens (top_level: t) (tokens: string Sequence.t) =
		match Sequence.next tokens with
		| Some (t, tail) -> (
			match exec_token top_level t with 
			| Error msg -> Error msg
			| Ok () -> exec_tokens top_level tail
		)
		| _ -> Ok ()

	let eval (top_level: t) (input: string) : string = 
		input 
			|> tokenize  
			|> Sequence.of_list
			|> exec_tokens top_level
			|> Result.map ~f: (fun _ -> to_string top_level)
			|> (function | Ok s -> s | Error s -> s)
end
