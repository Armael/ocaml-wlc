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

type touch_type =
  | Touch_Down
  | Touch_Up
  | Touch_Motion
  | Touch_Frame
  | Touch_Cancel

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

val origin_zero : origin
val size_zero : size
val geometry_zero : geometry

val origin_min : origin -> origin -> origin
val origin_max : origin -> origin -> origin

val size_min : size -> size -> size
val size_max : size -> size -> size

val geometry_contains : geometry -> geometry -> bool

type modifiers = {
  leds : led list;
  mods : modifier list;
}

module Output : sig
  type t

  val all : unit -> t list
  val get_focused : unit -> t
  val get_sleep : t -> bool
  val set_sleep : t -> bool -> unit
  val get_resolution : t -> size
  val set_resolution : t -> size -> unit
  val get_mask : t -> int list
  val set_mask : t -> int list -> unit

  val get_pixels :
    t ->
    ((int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t ->
     unit) ->
    unit

  val focus : t -> unit
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

  val all_of_output : Output.t -> t list
  val set_all_of_output : Output.t -> t list -> bool

  val focus : t option -> unit
  val close : t -> unit
  val get_output : t -> Output.t
  val set_output : t -> Output.t -> unit
  val send_to_back : t -> unit
  val send_below : t -> t -> unit
  val bring_above : t -> t -> unit
  val bring_to_front : t -> unit
  val get_mask : t -> int list
  val set_mask : t -> int list -> unit
  val get_geometry : t -> geometry
  val set_geometry : t -> geometry -> unit
  val get_type : t -> typ
  val set_type : t -> type_bit -> bool -> unit
  val get_state : t -> state
  val set_state : t -> state_bit -> bool -> unit
  val get_parent : t -> t option
  val set_parent : t -> t option -> unit
  val get_title : t -> string
  val set_title : t -> string -> unit
  val get_class : t -> string option
  val set_class : t -> string option -> unit
  val get_app_id : t -> string option
  val set_app_id : t -> string option -> bool
end

module Interface : sig
  type output = {
    created    : Output.t -> bool;
    destroyed  : Output.t -> unit;
    focus      : Output.t -> bool -> unit;
    resolution : Output.t -> size -> size -> unit;
  }

  type view_request = {
    geometry : View.t -> geometry -> unit;
    state    : View.t -> View.state_bit -> bool -> unit;
  }

  type view = {
    created        : View.t -> bool;
    destroyed      : View.t -> unit;
    focus          : View.t -> bool -> unit;
    move_to_output : View.t -> Output.t -> Output.t -> unit;
    request        : view_request;
  }

  type keyboard = {
    key : View.t option ->
      int -> modifiers -> int -> Keysym.t -> key_state -> bool;
  }

  type pointer = {
    button : View.t option -> int -> modifiers -> int -> button_state -> bool;
    scroll : View.t option ->
      int -> modifiers -> scroll_axis_bit list -> float * float -> bool;
    motion : View.t option -> int -> origin -> bool;
  }

  type touch = {
    touch : View.t option ->
      int -> modifiers -> touch_type -> int -> origin -> bool;
  }

  type t = {
    view     : view;
    keyboard : keyboard;
    pointer  : pointer;
    output   : output;
    touch    : touch;
  }
end

(* May raise Failure *)
val init : Interface.t -> unit
val terminate : unit -> unit
val run : unit -> unit

val log : string -> unit
