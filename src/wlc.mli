(** Bindings to the Wayland Compositor Library. *)

exception Failure

(** Translation of the header <xkbcommon/xkbcommon-keysyms.h>. 

    - Keysyms are translated to [int]s.
    - Keysyms names are mapped using the following schema:
      for each [XKB_KEY_something] definition in xkbcommon-keysyms.h
      corresponds a [Keysym._something] value.

      For example, [XKB_KEY_Return] translates to [Keysym._Return].

    Alternatively, see wlc__keysym.mli for the list of keysyms. *)
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

  (** Returns an integer identifier for this output. *)
  val id : t -> int64

  (** Get the list of outputs. *)
  val all : unit -> t list

  (** Get the currently focused output. *)
  val get_focused : unit -> t

  (** Get sleep state. *)
  val get_sleep : t -> bool

  (** Wake up / sleep. *)
  val set_sleep : t -> bool -> unit

  (** Get output resolution. *)
  val get_resolution : t -> size

  (** Set output resolution. *)
  val set_resolution : t -> size -> unit

  (** Get the list of currently visible spaces for this output.

      The current implementation of Wlc only handles 32 spaces per
      output. *)
  val get_mask : t -> int list

  (** Set the currently visible spaces for this output. *)
  val set_mask : t -> int list -> unit

  (** Get the pixels of the screen region corresponding to the output. *)
  val get_pixels :
    t ->
    ((int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t ->
     unit) ->
    unit

  (** Change the output focus. *)
  val focus : t option -> unit
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

  (** Returns an integer identifier for this view. *)
  val id : t -> int64

  (** Get all the views of an output, in stacking order. *)
  val all_of_output : Output.t -> t list

  (** Set the views of an output, also stacking order. 
      Useful to change the stack order or move many views at once. *)
  val set_all_of_output : Output.t -> t list -> bool

  (** Change the view focus. *)
  val focus : t option -> unit

  (** Close a view. *)
  val close : t -> unit

  (** Get the current output for this view. *)
  val get_output : t -> Output.t

  (** Set the output for this view. Alternatively you can use
      [Wlc.View.set_all_of_output]. *)
  val set_output : t -> Output.t -> unit

  (** Send behind everything. *)
  val send_to_back : t -> unit

  (** Send below another view. *)
  val send_below : t -> t -> unit

  (** Send above another view. *)
  val bring_above : t -> t -> unit

  (** Bring in front of everything. *)
  val bring_to_front : t -> unit

  (** Get the list of spaces where this view appears.

      The current implementation of Wlc only handles 32 spaces per
      output. *)
  val get_mask : t -> int list

  (** Set the list of spaces where the view appears. *)
  val set_mask : t -> int list -> unit

  (** Get the current geometry. *)
  val get_geometry : t -> geometry

  (** Set the geometry. *)
  val set_geometry : t -> geometry -> unit

  (** Get the type bitfield. *)
  val get_type : t -> typ

  (** Set a type bit. The boolean argument indicates whether it is set
      or not. *)
  val set_type : t -> type_bit -> bool -> unit

  (** Get the current state bitfield. *)
  val get_state : t -> state

  (** Set a state bit. The boolean argument idicates whether it is set
      or not. *)
  val set_state : t -> state_bit -> bool -> unit

  (** Get the parent view, if any. *)
  val get_parent : t -> t option

  (** Set the parent view. *)
  val set_parent : t -> t option -> unit

  (** Get the view title. *)
  val get_title : t -> string

  (** Set the view title. May raise [Failure]. *)
  val set_title : t -> string -> unit

  (** Get the view class (shell-surface only). *)
  val get_class : t -> string option

  (** Set the view class (shell-surface only). May raise [Failure]. *)
  val set_class : t -> string option -> unit

  (** Get the app id (xdg-surface only). *)
  val get_app_id : t -> string option

  (** Set the app id (xdg-surface only).  May raise [Failure]. *)
  val set_app_id : t -> string option -> unit
end

module Interface : sig
  (** Interface records for communicating with wlc.  A record of type
      [Wlc.Interface.t] should be instantiated with user-defined
      callbacks, then given to [Wlc.init]. *)

  type output = {
    created    : Output.t -> bool;
    (** An output was created. Return [false] if you want to destroy the output. *)
    destroyed  : Output.t -> unit;
    (** An output was destroyed. *)
    focus      : Output.t -> bool -> unit;
    (** An output got or lost focus. *)
    resolution : Output.t -> size -> size -> unit;
    (** An output resolution changed. *)
  }

  type view_request = {
    geometry : View.t -> geometry -> unit;
    (** Request to set a given geometry for a view. Apply using
        [Wlc.View.set_geometry] to agree. *)
    state    : View.t -> View.state_bit -> bool -> unit;
    (** Request to disable or enable the given state for a view. Apply
        using [Wlc.View.set_state] to agree. *)
  }

  type view = {
    created        : View.t -> bool;
    (** A view was created. Return [false] if you want to destroy the view. *)
    destroyed      : View.t -> unit;
    (** A view was destroyed. *)
    focus          : View.t -> bool -> unit;
    (** A view got or lost focus. *)
    move_to_output : View.t -> Output.t -> Output.t -> unit;
    (** A view was moved of output. *)
    
    request        : view_request;
  }

  type keyboard = {
    key : View.t option ->
      int -> modifiers -> int -> Keysym.t -> key_state -> bool;
    (** A key event was triggered, the first argument indicates the focused view. *)
  }

  type pointer = {
    button : View.t option -> int -> modifiers -> int -> button_state -> bool;
    (** A button event was triggered, the first argument indicates the focused view. *)
    scroll : View.t option ->
      int -> modifiers -> scroll_axis_bit list -> float * float -> bool;
    (** A scroll event was triggered, the first argument indicates the focused view. *)
    motion : View.t option -> int -> origin -> bool;
    (** A motion event was triggered, the first argument indicates the focused view. *)
  }

  type touch = {
    touch : View.t option ->
      int -> modifiers -> touch_type -> int -> origin -> bool;
    (** A touch event was tiggered, the first argument indicates the focused view. *)
  }

  type compositor = {
    ready : unit -> unit;
    (** The compositor is ready to accept clients. *)
  }

  (** The toplevel interface record. *)
  type t = {
    output     : output;
    view       : view;
    keyboard   : keyboard;
    pointer    : pointer;
    touch      : touch;
    compositor : compositor;
  }

  (** A dummy interface, where each handler does nothing. Useful for
      picking default callbacks. *)
  val dummy : t
end

(** Initialize wlc. May raise [Failure]. *)
val init : Interface.t -> unit

(** Terminate wlc. *)
val terminate : unit -> unit

(** Run the event loop. *)
val run : unit -> unit

(** Log a message. *)
val log : string -> unit
