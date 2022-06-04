open Base

type type_sig = Op of (elem list * elem list) | Apply
and elem = Poly of char | Mono of string | Quote of type_sig

(* literal equals, used for looking up in table *)
let rec type_sig_equal ts1 ts2 = match (ts1, ts2) with
	| (Op (is1, os1), Op (is2, os2)) -> 
		(List.equal elem_equal is1 is2) && (List.equal elem_equal os1 os2)
	| (Apply, Apply) -> true
	| _ -> false
and elem_equal e1 e2 = match (e1, e2) with 
	| (Poly c1, Poly c2) -> Char.equal c1 c2
	| (Mono s1, Mono s2) -> String.equal s1 s2
	| (Quote ts1, Quote ts2) -> type_sig_equal ts1 ts2
	| _ -> false

let rec elem_match input_elem popped_elem = match (input_elem, popped_elem) with
	| (Poly c, p) -> true
	| (Mono s1, Mono s2) -> String.equal s1 s2
	| (Quote ts1, Quote ts2) -> type_sig_match ts1 ts2
	| _ -> false
and type_sig_match input_ts popped_ts = match (input_ts, popped_ts) with
	| (Op (is1, os1), Op (is2, os2)) -> 
		(List.equal elem_match is1 is2) && (List.equal elem_match os1 os2)
	| (Apply, Apply) -> true
	| _ -> false

let push t = Op ([], [t])
let drop = Op([Poly 'a'], [])
let dup = Op([Poly 'a'], [Poly 'a'; Poly 'a'])
let quote ts = push (Quote ts)

let rec type_apply stack ts = match (ts, stack) with	
	| (Op (inputs, outputs), s) -> (
		let (popped, remaining) = List.split_n s (List.length inputs) in
		let map_elems i p = 
			if elem_match i p then Ok (i, p) else Error "no match"
		in
		let type_map = List.map2_exn inputs popped ~f:map_elems |> Result.all in
		let inner_lookup e tm = 
			List.Assoc.find tm ~equal:elem_equal e
			|> Result.of_option ~error: "cannot lookup element"
		in
		let lookup (e: elem): (elem, string) Result.t =  
			type_map |> Result.bind ~f:(inner_lookup e)
		in
		let new_outputs = List.map inputs ~f:lookup in
		new_outputs |> Result.all |> Result.map ~f: (fun nos -> nos @ remaining)
	)
	| (Apply, (Quote q_ts)::s) -> type_apply s q_ts
	| (Apply, _) -> Error "expected quote"

let infer tss =  
	List.fold_result tss ~init:[] ~f:type_apply |> Result.map ~f:List.rev
