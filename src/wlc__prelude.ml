open Ctypes
open Foreign

let (@::) o xs = match o with
  | Some x -> x::xs
  | None   ->    xs

let (&<) bits n =
  let open Unsigned.UInt32 in
  Infix.((bits land (one lsl n)) <> zero)

let some_if b x =
  if b then Some x else None

let ptr_eq p q = ptr_compare p q = 0

let list_of_mask32 (m: Unsigned.uint32): int list =
  let l = ref [] in
  for i = 31 downto 0 do
    l := (some_if (m &< i) i) @:: !l
  done; !l
	 
let mask32_of_list (l: int list): Unsigned.uint32 =
  let open Unsigned.UInt32 in
  List.fold_left (fun m i ->
		  logor m (shift_left one i))
		 zero l
