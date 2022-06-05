open Base

type word = Op of (elem list * elem list) | Apply
and elem = Poly of char | Mono of string | Quote of word

let rec elem_to_string = function
	| Poly c -> Printf.sprintf "'%c" c
	| Mono s -> s
	| Quote w -> word_to_string w
and word_to_string = function
	| Op (is, os) -> 
		let iss = List.map is ~f:elem_to_string |> String.concat ~sep: " " in
		let oss = List.map os ~f:elem_to_string |> String.concat ~sep: " " in
		(Printf.sprintf "%s -> %s" iss oss)
	| Apply -> "@"

(* literal equals, used for looking up in table *)
let rec word_equal ts1 ts2 = match (ts1, ts2) with
	| (Op (is1, os1), Op (is2, os2)) -> 
		(List.equal elem_equal is1 is2) && (List.equal elem_equal os1 os2)
	| (Apply, Apply) -> true
	| _ -> false
and elem_equal e1 e2 = match (e1, e2) with 
	| (Poly c1, Poly c2) -> Char.equal c1 c2
	| (Mono s1, Mono s2) -> String.equal s1 s2
	| (Quote ts1, Quote ts2) -> word_equal ts1 ts2
	| _ -> false

let rec elem_match input_elem popped_elem = match (input_elem, popped_elem) with
	| (Poly c, p) -> true
	| (Mono s1, Mono s2) -> String.equal s1 s2
	| (Quote w1, Quote w2) -> word_match w1 w2
	| _ -> false
and word_match input_ts popped_ts = match (input_ts, popped_ts) with
	| (Op (is1, os1), Op (is2, os2)) -> 
		(List.equal elem_match is1 is2) && (List.equal elem_match os1 os2)
	| (Apply, Apply) -> true
	| _ -> false

let push t = Op ([], [t])
let drop = Op([Poly 'a'], [])
let dup = Op([Poly 'a'], [Poly 'a'; Poly 'a'])
let quote ts = push (Quote ts)

let create_type_map inputs popped = 
		let map_elems i p = 
			if elem_match i p then Ok (i, p) else Error "no match"
		in
		List.map2_exn inputs popped ~f:map_elems |> Result.all

let lookup type_map e =
		let inner_lookup tm = 
			List.Assoc.find tm ~equal:elem_equal e
			|> Result.of_option ~error: 
				(Printf.sprintf "cannot lookup %s" (elem_to_string e))
		in
		type_map |> Result.bind ~f:inner_lookup

let rec type_apply stack ts = match (ts, stack) with	
	| (Op (inputs, outputs), s) -> (
		let (popped, remaining) = List.split_n s (List.length inputs) in
		let type_map = create_type_map inputs popped in
		let new_outputs = List.map outputs ~f:(lookup type_map) in
		new_outputs |> Result.all |> Result.map ~f: (fun nos -> nos @ remaining)
	)
	| (Apply, (Quote q_ts)::s) -> type_apply s q_ts
	| (Apply, _) -> Error "expected quote"

let infer tss =  
	List.fold_result tss ~init:[] ~f:type_apply |> Result.map ~f:List.rev
