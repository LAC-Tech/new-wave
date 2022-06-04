open Base

type type_sig = 
	| Push of elem
	| Drop
	| Dup
	| BinOp of (elem list * elem list)
	| Apply
and elem = Generic | Num | Symbol | Quote of type_sig

let elem_match l r = match (l, r) with
	| (Generic, _) -> true
	| (_, Generic) -> true
	| (a, b) -> true

let rec type_apply stack ts = match (ts, stack) with
	| (Push t, _) -> Ok (t::stack)
	
	| (Drop, t::s) -> Ok s
	| (Drop, _) -> Error "underflow"
	
	| (Dup, t::s) -> Ok (t::t::s)
	| (Dup, _) -> Error "underflow"
	
	| (BinOp (inputs, outputs), s) -> (
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
