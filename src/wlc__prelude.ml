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
