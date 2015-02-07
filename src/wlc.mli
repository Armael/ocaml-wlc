exception Failure

(* See wlc__keysym.mli for the list of keysyms *)
module Keysym : module type of struct include Wlc__keysym end

type modifier =
  | Shift
  | Caps
  | Ctrl
  | Alt
  | Mod2
  | Mod3
  | Logo
  | Mod5

type led =
  | Led_Num
  | Led_Caps
  | Led_Scroll

type key_state = Key_State_Released | Key_State_Pressed
type button_state = Button_State_Released | Button_State_Pressed

type scroll_axis_bit = Vertical | Horizontal

type origin = {
  x : int;
  y : int;
}

type size = {
  w : int;
  h : int;
}

type geometry = {
  origin : origin;
  size   : size;
}

type modifiers = {
  leds : led list;
  mods : modifier list;
}

module Space : sig
  type t

  val is_single : t -> bool

  val remove : t -> unit
end

module Output : sig
  type t

  val get_pixels :
    t ->
    ((int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t ->
     unit) ->
    unit

  val is_single : t -> bool

  val set_resolution : t -> size -> unit
  val get_resolution : t -> size
  val get_active_space : t -> Space.t
  val get_spaces : t -> Space.t list
  val focus_space : t -> Space.t -> unit
  val of_space : Space.t -> t
  (* may raise Failure (shouldn't happen, really, though) *)
  val add_space : t -> Space.t
end

module View : sig
  type state_bit =
    | Maximized
    | Fullscreen
    | Resizing
    | Moving
    | Activated

  type state = state_bit list

  type type_bit =
    | Override_Redirect
    | Unmanaged
    | Splash
    | Modal
    | Popup

  type typ = type_bit list

  type t

  val is_single : t -> bool

  val set_title : t -> string -> unit
  val get_title : t -> string
  val set_class : t -> string option -> unit
  val get_class : t -> string option
  val set_space : t -> Space.t -> unit
  val get_space : t -> Space.t
  val all_of_space : Space.t -> t list
  val get_type : t -> typ
  val get_state : t -> state
  val set_state : t -> state_bit -> bool -> unit
  val get_geometry : t -> geometry
  val set_geometry : t -> geometry -> unit
  val close : t -> unit
  val send_below : t -> t -> unit
  val send_to_back : t -> unit
  val bring_above : t -> t -> unit
  val bring_to_front : t -> unit
  val set_parent : t -> t option -> unit
  val get_parent : t -> t option
end

module Compositor : sig
  type t

  val get_outputs : t -> Output.t list
  val get_focused_output : t -> Output.t
  val get_focused_space : t -> Space.t
  val focus_view : t -> View.t option -> unit
  val focus_output : t -> Output.t -> unit
  (* May raise Failure *)
  val create : unit -> t
end

module Interface : sig
  type view_request = {
    geometry : Compositor.t -> View.t -> geometry -> unit;
    state    : Compositor.t -> View.t -> View.state_bit -> bool -> unit;
  }

  type view = {
    created      : Compositor.t -> View.t -> Space.t -> bool;
    destroyed    : Compositor.t -> View.t -> unit;
    switch_space : Compositor.t -> View.t -> Space.t -> Space.t -> unit;
    request      : view_request;
  }

  type keyboard = {
    key : Compositor.t ->
      View.t option -> int -> modifiers -> int -> Keysym.t -> key_state -> bool;
  }

  type pointer = {
    button :
      Compositor.t ->
      View.t option -> int -> modifiers -> int -> button_state -> bool;
    scroll :
      Compositor.t ->
      View.t option ->
      int -> modifiers -> scroll_axis_bit list -> float * float -> bool;
    motion : Compositor.t -> View.t option -> int -> origin -> bool;
  }

  type output = {
    created    : Compositor.t -> Output.t -> bool;
    destroyed  : Compositor.t -> Output.t -> unit;
    activated  : Compositor.t -> Output.t -> unit;
    resolution : Compositor.t -> Output.t -> size -> unit;
  }

  type space = {
    created   : Compositor.t -> Space.t -> bool;
    destroyed : Compositor.t -> Space.t -> unit;
    activated : Compositor.t -> Space.t -> unit;
  }

  type t = {
    view     : view;
    keyboard : keyboard;
    pointer  : pointer;
    output   : output;
    space    : space;
  }
end

(* May raise Failure *)
val init : Interface.t -> unit
val terminate : unit -> unit
val run : unit -> unit

val log : string -> unit
