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
      
let bool = view
  ~read:(fun x -> x <> 0)
  ~write:(fun b -> if b then 1 else 0)
  int

let ptr_eq p q = ptr_compare p q = 0
