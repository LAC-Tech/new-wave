open Base

type elem = Generic | Num | Symbol | Quote of (elem list -> elem list)

let elem_match l r = match (l, r) with
	| (Generic, _) -> true
	| (_, Generic) -> true
	| (A, B) -> false

exception TypeErr of string

type type_sig = 
	| Push of elem
	| Drop
	| Dup
	| BinOp of (elem list * elem list)
	| Quote_ of type_sig
	| Apply

let rec type_apply ts stack = match (ts, stack) with
	| (Push t, _) -> t::stack
	
	| (Drop, t::s) -> s
	| (Drop, _) -> raise (TypeErr "underflow")
	
	| (Dup, t::s) -> t::t::s
	| (Dup, _) -> raise (TypeErr "underflow")
	
	| (BinOp (inputs, outputs), a::b::s) -> (
		if (List.equal elem_match [a; b] inputs) then 
			outputs @ s
		else
			raise (TypeErr "Expected and received are different")
	)
	| (BinOp (_, _), _) -> raise (TypeErr "underflow")
	
	| (Quote_ word, s) -> (Quote (type_apply word))::s

	| (Apply, (Quote f)::s) -> f s
	| (Apply, _) -> raise (TypeErr "expected quote")

let infer tss = 
	List.fold_left (fun accum ts -> type_apply ts accum) [] tss |> List.rev
