open Ctypes
open Foreign
open Prelude

let wl_lib = Dl.dlopen
  ~filename:"libwayland-server.so"
  ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

module C = struct
  type list
  let list : list structure typ = structure "wl_list"
  let prev = field list "prev" (ptr list)
  let next = field list "next" (ptr list)
  let () = seal list
end

type wl_list = C.list structure ptr
let wl_list = ptr C.list

let prev l = getf (!@ l) C.prev
let next l = getf (!@ l) C.next

let ocaml_of_wl_list extract_elt l =
  let rec aux acc elt =
    if ptr_eq elt l then List.rev acc
    else aux ((extract_elt elt)::acc) (next elt)
  in
  
  if ptr_eq (coerce wl_list (ptr void) l) null then []
  else aux [] (next l)
