open Base

type type_sig = Op of (elem list * elem list) | Apply
and elem = Poly of char | Mono of string | Quote of type_sig

let elem_match l r = match (l, r) with
	| (Poly _, _) -> true
	| (_, Poly _) -> true
	| (a, b) -> true

let push t = Op ([], [t])
let drop = Op([Poly 'a'], [])
let dup = Op([Poly 'a'], [Poly 'a'; Poly 'a'])
let quote ts = push (Quote ts)

let rec type_apply stack ts = match (ts, stack) with	
	| (Op (inputs, outputs), s) -> (
		let (popped, remaining) = List.split_n s (List.length inputs) in
		if (List.equal elem_match popped inputs) then 
			Ok (outputs @ remaining)
		else
			Error "Expected and received are different"
	)
	| (Apply, (Quote q_ts)::s) -> type_apply s q_ts
	| (Apply, _) -> Error "expected quote"

let infer tss =  
	List.fold_result tss ~init:[] ~f:type_apply |> Result.map ~f:List.rev
