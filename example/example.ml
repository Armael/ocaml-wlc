open Wlc

let get_topmost (output: Output.t) (offset: int): View.t option =
  let views = View.all_of_output output in
  let n = List.length views in
  if n = 0 then None
  else Some (List.nth views ((n - 1 + offset) mod n))

let relayout (output: Output.t) =
  let r = Output.get_resolution output in
  let views = View.all_of_output output in
  let n = List.length views in

  let w = r.w / 2 in
  let h = r.h / (max ((1 + n) / 2) 1) in
  List.fold_left (fun (toggle, i, y) view ->
    let g = {
      origin = {
        x = if toggle then w else 0;
        y;
      };
      size = {
        w = if not toggle && i = n - 1 then r.w else w;
        h;
      }
    } in
    View.set_geometry view g;
    (not toggle, i+1, y + (if toggle then h else 0))
  ) (false, 0, 0) views |> ignore

(* Handlers *)
let output_created output =
  Printf.printf "created output (%Lu)\n" (Output.id output);
  true

let output_destroyed output =
  Printf.printf "destroyed output (%Lu)\n" (Output.id output)

let output_resolution output size_from size_to =
  relayout output

let view_created view =
  Printf.printf "created view (%Lu)\n" (View.id view);
  View.bring_to_front view;
  View.focus (Some view);
  relayout (View.get_output view);
  true

let view_destroyed view =
  Printf.printf "destroyed view (%Lu)\n" (View.id view);
  View.focus (get_topmost (View.get_output view) 0);
  relayout (View.get_output view)

let view_focus view focus =
  View.set_state view View.Activated focus

let keyboard_key view time modifiers key sym state =
  if state = Key_State_Pressed then
    begin match view with
    | Some view ->
      if List.mem Ctrl modifiers.mods then (
        if sym = Keysym._q then
          View.close view
        else if sym = Keysym._Down then (
          View.send_to_back view;
          View.focus (get_topmost (View.get_output view) 0)
        )
      ); false
      
    | None ->
      if List.mem Ctrl modifiers.mods && sym = Keysym._Escape then (
        Wlc.terminate ()
      ); false
    end
  else true

let pointer_button view time modifiers button state =
  if state = Button_State_Pressed then
    View.focus view;
  true

let () =
  let open Interface in
  let interface = {
    dummy with

    output = {
      created = output_created;
      destroyed = output_destroyed;
      focus = dummy.output.focus;
      resolution = output_resolution;
    };

    view = {
      dummy.view with
      created = view_created;
      destroyed = view_destroyed;
      focus = view_focus;
    };

    keyboard = { key = keyboard_key; };

    pointer = {
      dummy.pointer with
      button = pointer_button;
    };
  } in

  Wlc.init interface;
  Wlc.run ()
