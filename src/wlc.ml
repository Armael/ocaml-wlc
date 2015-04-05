open Ctypes
open Foreign
open Wlc__prelude

module Wl = Wlc__wl

exception Failure

let fail_if_null ty p =
  if ptr_eq (coerce ty (ptr void) p) null then
    raise Failure
  else
    p

let handle_of_opt = function
  | Some h -> h
  | None -> null

let opt_of_handle h =
  if ptr_eq h null then None
  else Some h

module Keysym = Wlc__keysym

let wlc_lib = Dl.dlopen
    ~filename:"libwlc.so"
    ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

module C = struct
  module Origin = struct
    type origin
    let origin : origin structure typ = structure "wlc_origin"
    let x = field origin "x" int32_t
    let y = field origin "y" int32_t
    let () = seal origin
  end

  module Size = struct
    type size
    let size : size structure typ = structure "wlc_size"
    let w = field size "w" uint32_t
    let h = field size "h" uint32_t
    let () = seal size
  end

  module Geometry = struct
    type geometry
    let geometry : geometry structure typ = structure "wlc_geometry"
    let origin = field geometry "origin" Origin.origin
    let size = field geometry "size" Size.size
    let () = seal geometry
  end

  module Modifiers = struct
    type modifiers
    let modifiers : modifiers structure typ = structure "wlc_modifiers"
    let leds = field modifiers "leds" uint32_t
    let mods = field modifiers "mods" uint32_t
    let () = seal modifiers
  end
end

type modifier =
  | Shift
  | Caps
  | Ctrl
  | Alt
  | Mod2
  | Mod3
  | Logo
  | Mod5

let modifier_of_c bits =
  (some_if (bits &< 0) Shift)
  @:: (some_if (bits &< 1) Caps)
  @:: (some_if (bits &< 2) Ctrl)
  @:: (some_if (bits &< 3) Alt)
  @:: (some_if (bits &< 4) Mod2)
  @:: (some_if (bits &< 5) Mod3)
  @:: (some_if (bits &< 6) Logo)
  @:: (some_if (bits &< 7) Mod5)
  @:: []

let c_of_modifier modif =
  let open Unsigned.UInt32 in
  let open Infix in
  List.fold_left (fun bits flag -> match flag with
    | Shift -> bits lor one
    | Caps -> bits lor (one lsl 1)
    | Ctrl -> bits lor (one lsl 2)
    | Alt -> bits lor (one lsl 3)
    | Mod2 -> bits lor (one lsl 4)
    | Mod3 -> bits lor (one lsl 5)
    | Logo -> bits lor (one lsl 6)
    | Mod5 -> bits lor (one lsl 7)
  ) zero modif

let modifier_list = view ~read:modifier_of_c ~write:c_of_modifier uint32_t

type led =
  | Led_Num
  | Led_Caps
  | Led_Scroll

let led_of_c bits =
  (some_if (bits &< 0) Led_Num)
  @:: (some_if (bits &< 1) Led_Caps)
  @:: (some_if (bits &< 2) Led_Scroll)
  @:: []

let c_of_led led =
  let open Unsigned.UInt32 in
  let open Infix in
  List.fold_left (fun bits flag -> match flag with
    | Led_Num -> bits lor one
    | Led_Caps -> bits lor (one lsl 1)
    | Led_Scroll -> bits lor (one lsl 2)
  ) zero led

let led_list = view ~read:led_of_c ~write:c_of_led uint32_t

type key_state =
  | Key_State_Released
  | Key_State_Pressed

let key_state = view
    ~read:(fun i -> if i = Unsigned.UInt32.zero then Key_State_Released
            else Key_State_Pressed)
    ~write:(function
      | Key_State_Released -> Unsigned.UInt32.zero
      | Key_State_Pressed -> Unsigned.UInt32.one)
    uint32_t

type button_state =
  | Button_State_Released
  | Button_State_Pressed

let button_state = view
    ~read:(fun i -> if i = Unsigned.UInt32.zero then Button_State_Released
            else Button_State_Pressed)
    ~write:(function
      | Button_State_Released -> Unsigned.UInt32.zero
      | Button_State_Pressed -> Unsigned.UInt32.one)
    uint32_t

type scroll_axis_bit =
  | Vertical
  | Horizontal

let scroll_axis =
  let open Unsigned.UInt32 in
  let open Infix in
  view
    ~read:(fun i ->
      let i = of_int (Unsigned.UInt8.to_int i) in
      (some_if (i &< 0) Vertical)
      @:: (some_if (i &< 1) Horizontal)
      @:: [])
    ~write:(fun x ->
      List.fold_left (fun bits flag -> match flag with
        | Vertical -> bits lor one
        | Horizontal -> bits lor (one lsl 1)
      ) zero x
      |> to_int |> Unsigned.UInt8.of_int)
    uint8_t

type touch_type =
  | Touch_Down
  | Touch_Up
  | Touch_Motion
  | Touch_Frame
  | Touch_Cancel

let touch_type_of_c bit =
  match Unsigned.UInt32.to_int bit with
  | 0 -> Touch_Down
  | 1 -> Touch_Up
  | 2 -> Touch_Motion
  | 3 -> Touch_Frame
  | _ -> Touch_Cancel

let c_of_touch_type bit =
  (match bit with
   | Touch_Down -> 0
   | Touch_Up -> 1
   | Touch_Motion -> 2
   | Touch_Frame -> 3
   | Touch_Cancel -> 4)
  |> Unsigned.UInt32.of_int

let touch_type = view ~read:touch_type_of_c ~write:c_of_touch_type uint32_t

type origin = {
  x : int;
  y : int;
}

let origin_of_c c_origin = {
  x = getf (!@ c_origin) C.Origin.x |> Int32.to_int;
  y = getf (!@ c_origin) C.Origin.y |> Int32.to_int;
}

let c_of_origin origin =
  let c_origin = make C.Origin.origin in
  setf c_origin C.Origin.x (Int32.of_int origin.x);
  setf c_origin C.Origin.y (Int32.of_int origin.y);
  addr c_origin

let origin : origin typ = view
    ~read:origin_of_c
    ~write:c_of_origin
    (ptr C.Origin.origin)

type size = {
  w : int;
  h : int;
}

let size_of_c c_size = {
  w = getf (!@ c_size) C.Size.w |> Unsigned.UInt32.to_int;
  h = getf (!@ c_size) C.Size.h |> Unsigned.UInt32.to_int;
}

let c_of_size size =
  let c_size = make C.Size.size in
  setf c_size C.Size.w (Unsigned.UInt32.of_int size.w);
  setf c_size C.Size.h (Unsigned.UInt32.of_int size.h);
  addr c_size

let size : size typ = view
    ~read:size_of_c
    ~write:c_of_size
    (ptr C.Size.size)

type geometry = {
  origin : origin;
  size   : size;
}

let geometry_of_c c_geo = {
  origin = c_geo |-> C.Geometry.origin |> origin_of_c;
  size = c_geo |-> C.Geometry.size |> size_of_c;
}

let c_of_geometry geo =
  let c_geo = make C.Geometry.geometry in
  setf c_geo C.Geometry.origin (!@ (c_of_origin geo.origin));
  setf c_geo C.Geometry.size (!@ (c_of_size geo.size));
  addr c_geo

let geometry : geometry typ = view
    ~read:geometry_of_c
    ~write:c_of_geometry
    (ptr C.Geometry.geometry)

(* These reflect the definitions of geometry.h; however they are
   defined directly in OCaml for simplicity.

   Maybe this is a bad idea. Let's just hope these definitions do not
   change often in wlc. *)
let origin_zero = { x = 0; y = 0 }
let size_zero = { w = 0; h = 0 }
let geometry_zero = { origin = origin_zero; size = size_zero }

let origin_min a b = {
  x = min a.x b.x;
  y = min a.y b.y;
}
let origin_max a b = {
  x = max a.x b.x;
  y = max a.y b.y;
}

let size_min a b = {
  w = min a.w b.w;
  h = min a.h b.h;
}
let size_max a b = {
  w = max a.w b.w;
  h = max a.h b.h;
}

let geometry_contains a b =
  a.origin.x <= b.origin.x && a.origin.y <= b.origin.y &&
  a.origin.x + a.size.w >= b.origin.x + b.size.w &&
  a.origin.y + a.size.h >= b.origin.y + b.size.h

					  
type modifiers = {
  leds : led list;
  mods : modifier list;
}

let modifiers_of_c c_mods = {
  leds = getf (!@ c_mods) C.Modifiers.leds |> led_of_c;
  mods = getf (!@ c_mods) C.Modifiers.mods |> modifier_of_c;
}

let c_of_modifiers mods =
  let c_mods = make C.Modifiers.modifiers in
  setf c_mods C.Modifiers.leds (c_of_led mods.leds);
  setf c_mods C.Modifiers.mods (c_of_modifier mods.mods);
  addr c_mods

let modifiers : modifiers typ = view
    ~read:modifiers_of_c
    ~write:c_of_modifiers
    (ptr C.Modifiers.modifiers)

type handle = unit ptr
let handle = ptr void

module Output = struct
  type t = handle
  let t = handle

  let all () =
    let n_ptr = allocate size_t (Unsigned.Size_t.of_int 0) in
    let buf = foreign ~from:wlc_lib "wlc_get_outputs"
		      (ptr size_t @-> returning (ptr t))
		      n_ptr
    in
    let a = CArray.from_ptr buf (!@ n_ptr |> Unsigned.Size_t.to_int) in
    CArray.to_list a

  let get_focused = foreign ~from:wlc_lib "wlc_get_focused_output"
			    (void @-> returning t)

  let get_sleep = foreign ~from:wlc_lib "wlc_output_get_sleep"
			  (t @-> returning bool)

  let set_sleep = foreign ~from:wlc_lib "wlc_output_set_sleep"
			  (t @-> bool @-> returning void)

  let get_resolution = foreign ~from:wlc_lib "wlc_output_get_resolution"
      (t @-> returning size)

  let set_resolution = foreign ~from:wlc_lib "wlc_output_set_resolution"
      (t @-> size @-> returning void)

  let get_mask output =
    foreign ~from:wlc_lib "wlc_output_get_mask" (t @-> returning uint32_t)
	    output
    |> list_of_mask32

  let set_mask output m =
    foreign ~from:wlc_lib "wlc_output_set_mask" (t @-> uint32_t @-> returning void)
	    output (mask32_of_list m)

  let get_pixels output async =
    foreign ~from:wlc_lib "wlc_output_get_pixels"
      (t @-> (funptr (size @-> ptr uint8_t @-> ptr void @-> returning void))
	 @-> ptr void
         @-> returning void)
      output
      (fun s buf _ ->
         let buf = coerce (ptr uint8_t) (ptr int) buf in
         async (bigarray_of_ptr array2 (s.w, s.h) Bigarray.int8_unsigned buf))
      null

  let focus = foreign ~from:wlc_lib "wlc_output_focus" (t @-> returning void)
end

module View = struct
  type t = handle
  let t = handle
  
  type state_bit =
    | Maximized
    | Fullscreen
    | Resizing
    | Moving
    | Activated

  let state_bit_of_c bit =
    match Unsigned.UInt32.to_int bit with
    | 1 -> Maximized
    | 2 -> Fullscreen
    | 4 -> Resizing
    | 8 -> Moving
    | _ -> Activated

  let c_of_state_bit bit =
    (match bit with
     | Maximized -> 1
     | Fullscreen -> 2
     | Resizing -> 4
     | Moving -> 8
     | Activated -> 16)
    |> Unsigned.UInt32.of_int

  let state_bit = view ~read:state_bit_of_c ~write:c_of_state_bit uint32_t

  type state = state_bit list

  let state_of_c bits =
    (some_if (bits &< 0) Maximized)
    @:: (some_if (bits &< 1) Fullscreen)
    @:: (some_if (bits &< 2) Resizing)
    @:: (some_if (bits &< 3) Moving)
    @:: (some_if (bits &< 4) Activated)
    @:: []

  let c_of_state state =
    let open Unsigned.UInt32 in
    let open Infix in
    List.fold_left (fun bits flag -> match flag with
      | Maximized -> bits lor one
      | Fullscreen -> bits lor (one lsl 1)
      | Resizing -> bits lor (one lsl 2)
      | Moving -> bits lor (one lsl 3)
      | Activated -> bits lor (one lsl 4)
    ) zero state

  let state = view ~read:state_of_c ~write:c_of_state uint32_t

  type type_bit =
    | Override_Redirect
    | Unmanaged
    | Splash
    | Modal
    | Popup

  let type_bit_of_c bit =
    match Unsigned.UInt32.to_int bit with
    | 1 -> Override_Redirect
    | 2 -> Unmanaged
    | 4 -> Splash
    | 8 -> Modal
    | _ -> Popup

  let c_of_type_bit bit =
    (match bit with
     | Override_Redirect -> 1
     | Unmanaged -> 2
     | Splash -> 4
     | Modal -> 8
     | Popup -> 16)
    |> Unsigned.UInt32.of_int

  let type_bit = view ~read:type_bit_of_c ~write:c_of_type_bit uint32_t

  type typ = type_bit list

  let typ_of_c bits =
    (some_if (bits &< 0) Override_Redirect)
    @:: (some_if (bits &< 1) Unmanaged)
    @:: (some_if (bits &< 2) Splash)
    @:: (some_if (bits &< 3) Modal)
    @:: (some_if (bits &< 4) Popup)
    @:: []

  let c_of_typ typ =
    let open Unsigned.UInt32 in
    let open Infix in
    List.fold_left (fun bits flag -> match flag with
      | Override_Redirect -> bits lor one
      | Unmanaged -> bits lor (one lsl 1)
      | Splash -> bits lor (one lsl 2)
      | Modal -> bits lor (one lsl 3)
      | Popup -> bits lor (one lsl 4)
    ) zero typ

  let typ = view ~read:typ_of_c ~write:c_of_typ uint32_t

  let all_of_output output =
    let n_ptr = allocate size_t (Unsigned.Size_t.of_int 0) in
    let buf = foreign ~from:wlc_lib "wlc_output_get_views"
		      (Output.t @-> ptr size_t @-> returning (ptr t))
		      output
		      n_ptr
    in
    let a = CArray.from_ptr buf (!@ n_ptr |> Unsigned.Size_t.to_int) in
    CArray.to_list a

  let set_all_of_output output views =
    let n = List.length views |> Unsigned.Size_t.of_int in
    let buf = CArray.of_list t views |> CArray.start in
    foreign ~from:wlc_lib "wlc_output_set_views"
	    (Output.t @-> (ptr t) @-> size_t @-> returning bool)
	    output
	    buf
	    n

  let focus view =
    foreign ~from:wlc_lib "wlc_view_focus"
	    (t @-> returning void)
	    (handle_of_opt view)

  let close = foreign ~from:wlc_lib "wlc_view_close"
      (t @-> returning void)

  let get_output = foreign ~from:wlc_lib "wlc_view_get_output"
			   (t @-> returning Output.t)

  let set_output = foreign ~from:wlc_lib "wlc_view_set_output"
			   (t @-> Output.t @-> returning void)

  let send_to_back = foreign ~from:wlc_lib "wlc_view_send_to_back"
      (t @-> returning void)

  let send_below = foreign ~from:wlc_lib "wlc_view_send_below"
      (t @-> t @-> returning void)

  let bring_above = foreign ~from:wlc_lib "wlc_view_bring_above"
      (t @-> t @-> returning void)

  let bring_to_front = foreign ~from:wlc_lib "wlc_view_bring_to_front"
      (t @-> returning void)

  let get_mask view =
    foreign ~from:wlc_lib "wlc_view_get_mask"
	    (t @-> returning uint32_t)
	    view 
    |> list_of_mask32

  let set_mask view m =
    foreign ~from:wlc_lib "wlc_view_set_mask"
	    (t @-> uint32_t @-> returning void)
	    view
	    (mask32_of_list m)

  let get_geometry = foreign ~from:wlc_lib "wlc_view_get_geometry"
      (t @-> returning geometry)

  let set_geometry = foreign ~from:wlc_lib "wlc_view_set_geometry"
      (t @-> geometry @-> returning void)

  let get_type = foreign ~from:wlc_lib "wlc_view_get_type"
			 (t @-> returning typ)

  let set_type = foreign ~from:wlc_lib "wlc_view_set_type"
			 (t @-> type_bit @-> bool @-> returning void)

  let get_state = foreign ~from:wlc_lib "wlc_view_get_state"
      (t @-> returning state)

  let set_state = foreign ~from:wlc_lib "wlc_view_set_state"
      (t @-> state_bit @-> bool @-> returning void)

  let get_parent view =
    foreign ~from:wlc_lib "wlc_view_get_parent"
      (t @-> returning t)
      view
    |> opt_of_handle

  let set_parent view parent =
    foreign ~from:wlc_lib "wlc_view_set_parent"
      (t @-> t @-> returning void)
      view
      (handle_of_opt parent)

  let get_title = foreign ~from:wlc_lib "wlc_view_get_title"
      (t @-> returning string)

  let set_title = foreign ~from:wlc_lib "wlc_view_set_title"
      (t @-> string @-> returning void)

  let get_class = foreign ~from:wlc_lib "wlc_view_get_class"
      (t @-> returning string_opt)

  let set_class = foreign ~from:wlc_lib "wlc_view_set_class"
			  (t @-> string_opt @-> returning void)

  let get_app_id = foreign ~from:wlc_lib "wlc_view_get_app_id"
			   (t @-> returning string_opt)

  let set_app_id = foreign ~from:wlc_lib "wlc_view_set_app_id"
			   (t @-> string_opt @-> returning bool)
end

module Interface = struct
  module C = struct
    module Interface_Output = struct
      type output
      let output : output structure typ = structure "output"
      let created = field output "created"
          (funptr (Output.t @-> returning bool))
      let destroyed = field output "destroyed"
          (funptr (Output.t @-> returning void))
      let focus = field output "focus"
          (funptr (Output.t @-> bool @-> returning void))
      let resolution = field output "resolution"
          (funptr (Output.t @-> size @-> size @-> returning void))
      let () = seal output
    end

    module Interface_View = struct
      module Request = struct
        type request
        let request : request structure typ = structure "request"
        let geometry = field request "geometry"
            (funptr (View.t @-> geometry @-> returning void))
        let state = field request "state"
            (funptr (View.t @-> View.state_bit @-> bool
                     @-> returning void))
        let () = seal request
      end

      type view
      let view : view structure typ = structure "view"
      let created = field view "created"
          (funptr (View.t @-> returning bool))
      let destroyed = field view "destroyed"
          (funptr (View.t @-> returning void))
      let focus = field view "focus"
			(funptr (View.t @-> bool @-> returning void))
      let move_to_output = field view "move_to_output"
				 (funptr (View.t @-> Output.t @-> Output.t @-> returning void))
      let request = field view "request" Request.request
      let () = seal view
    end

    module Interface_Keyboard = struct
      type keyboard
      let keyboard : keyboard structure typ = structure "keyboard"
      let key = field keyboard "key"
          (funptr (View.t @-> uint32_t
                   @-> modifiers @-> uint32_t @-> uint32_t
                   @-> key_state @-> returning bool))
      let () = seal keyboard
    end

    module Interface_Pointer = struct
      type pointer
      let pointer : pointer structure typ = structure "pointer"
      let button = field pointer "button"
          (funptr (View.t @-> uint32_t
                   @-> modifiers @-> uint32_t
                   @-> button_state @-> returning bool))
      let scroll = field pointer "scroll"
          (funptr (View.t @-> uint32_t
                   @-> modifiers @-> scroll_axis
                   @-> ptr double @-> returning bool))
      let motion = field pointer "motion"
          (funptr (View.t @-> uint32_t
                   @-> origin @-> returning bool))
      let () = seal pointer
    end

    module Interface_Touch = struct
      type touch
      let touch : touch structure typ = structure "touch"
      let touchf = field touch "touch"
			 (funptr (View.t @-> uint32_t
				  @-> modifiers @-> touch_type
				  @-> uint32_t @-> origin
				  @-> returning bool))
      let () = seal touch
    end

    type interface
    let interface : interface structure typ = structure "wlc_interface"
    let output = field interface "output" Interface_Output.output
    let view = field interface "view" Interface_View.view
    let keyboard = field interface "keyboard" Interface_Keyboard.keyboard
    let pointer = field interface "pointer" Interface_Pointer.pointer
    let touch = field interface "touch" Interface_Touch.touch
    let () = seal interface
  end

  type output = {
    created    : Output.t -> bool;
    destroyed  : Output.t -> unit;
    focus      : Output.t -> bool -> unit;
    resolution : Output.t -> size -> size -> unit;
  }

  let io_of_c c_io = {
    created = getf (!@ c_io) C.Interface_Output.created;
    destroyed = getf (!@ c_io) C.Interface_Output.destroyed;
    focus = getf (!@ c_io) C.Interface_Output.focus;
    resolution = getf (!@ c_io) C.Interface_Output.resolution;
  }

  let c_of_io io =
    let c_io = make C.Interface_Output.output in
    setf c_io C.Interface_Output.created io.created;
    setf c_io C.Interface_Output.destroyed io.destroyed;
    setf c_io C.Interface_Output.focus io.focus;
    setf c_io C.Interface_Output.resolution io.resolution;
    addr c_io

  let output : output typ = Ctypes.view
      ~read:io_of_c
      ~write:c_of_io
      (ptr C.Interface_Output.output)

  type view_request = {
    geometry : View.t -> geometry -> unit;
    state    : View.t -> View.state_bit -> bool -> unit;
  }

  let ivr_of_c c_ivr = {
    geometry = getf (!@ c_ivr) C.Interface_View.Request.geometry;
    state = getf (!@ c_ivr) C.Interface_View.Request.state;
  }

  let c_of_ivr ivr =
    let c_ivr = make C.Interface_View.Request.request in
    setf c_ivr C.Interface_View.Request.geometry ivr.geometry;
    setf c_ivr C.Interface_View.Request.state ivr.state;
    addr c_ivr

  let view_request : view_request typ = view
      ~read:ivr_of_c
      ~write:c_of_ivr
      (ptr C.Interface_View.Request.request)

  type view = {
    created        : View.t -> bool;
    destroyed      : View.t -> unit;
    focus          : View.t -> bool -> unit;
    move_to_output : View.t -> Output.t -> Output.t -> unit;

    request      : view_request;
  }

  let iv_of_c c_iv = {
    created = getf (!@ c_iv) C.Interface_View.created;
    destroyed = getf (!@ c_iv) C.Interface_View.destroyed;
    focus = getf (!@ c_iv) C.Interface_View.focus;
    move_to_output = getf (!@ c_iv) C.Interface_View.move_to_output;
    request = c_iv |-> C.Interface_View.request |> ivr_of_c;
  }

  let c_of_iv iv =
    let c_iv = make C.Interface_View.view in
    setf c_iv C.Interface_View.created iv.created;
    setf c_iv C.Interface_View.destroyed iv.destroyed;
    setf c_iv C.Interface_View.focus iv.focus;
    setf c_iv C.Interface_View.move_to_output iv.move_to_output;
    setf c_iv C.Interface_View.request (!@ (c_of_ivr iv.request));
    addr c_iv

  let view : view typ = view
      ~read:iv_of_c
      ~write:c_of_iv
      (ptr C.Interface_View.view)

  type keyboard = {
    key : View.t option -> int -> modifiers -> int -> Keysym.t -> key_state -> bool;
  }

  let ik_of_c c_ik = {
    key =
      let f = getf (!@ c_ik) C.Interface_Keyboard.key in
      fun v time modifiers key sym st ->
        let open Unsigned.UInt32 in
        f (handle_of_opt v)
          (of_int time) modifiers
          (of_int key) (of_int sym)
          st;
  }

  let c_of_ik ik =
    let c_ik = make C.Interface_Keyboard.keyboard in
    setf c_ik C.Interface_Keyboard.key
      (fun v time modifiers key sym st ->
         let open Unsigned.UInt32 in
         ik.key (opt_of_handle v)
           (to_int time) modifiers
           (to_int key) (to_int sym)
           st);
    addr c_ik

  let keyboard : keyboard typ = Ctypes.view
      ~read:ik_of_c
      ~write:c_of_ik
      (ptr C.Interface_Keyboard.keyboard)

  type pointer = {
    button : View.t option -> int -> modifiers -> int ->
      button_state -> bool;
    scroll : View.t option -> int -> modifiers -> scroll_axis_bit list ->
      float * float -> bool;
    motion : View.t option -> int -> origin -> bool;
  }

  let ip_of_c c_ip =
    let open Unsigned.UInt32 in
    let c_button = getf (!@ c_ip) C.Interface_Pointer.button in
    let c_scroll = getf (!@ c_ip) C.Interface_Pointer.scroll in
    let c_motion = getf (!@ c_ip) C.Interface_Pointer.motion in
    {
      button = (fun v time modifiers button st ->
        c_button (handle_of_opt v)
          (of_int time) modifiers (of_int button) st);
      scroll = (fun v time modifiers axis (a, b) ->
        let amount = CArray.of_list double [a; b] |> CArray.start in
        c_scroll (handle_of_opt v) (of_int time) modifiers axis amount);
      motion = (fun v time origin ->
        c_motion (handle_of_opt v) (of_int time) origin);
    }

  let c_of_ip ip =
    let open Unsigned.UInt32 in
    let c_ip = make C.Interface_Pointer.pointer in
    setf c_ip C.Interface_Pointer.button
      (fun v time modifiers button st ->
         ip.button (opt_of_handle v)
           (to_int time) modifiers
           (to_int button)
           st);
    setf c_ip C.Interface_Pointer.scroll
      (fun v time modifiers axis amount ->
         let arr = CArray.from_ptr amount 2 in
         ip.scroll (opt_of_handle v)
           (to_int time) modifiers axis
           (CArray.get arr 0, CArray.get arr 1));
    setf c_ip C.Interface_Pointer.motion
      (fun v time origin ->
         ip.motion (opt_of_handle v) (to_int time) origin);
    addr c_ip

  let pointer : pointer typ = Ctypes.view
      ~read:ip_of_c
      ~write:c_of_ip
      (ptr C.Interface_Pointer.pointer)

  type touch = {
    touch : View.t option -> int -> modifiers -> touch_type -> int -> origin -> bool;
  }

  let it_of_c c_it = {
    touch =
      let f = getf (!@ c_it) C.Interface_Touch.touchf in
      fun v time modifiers typ slot origin ->
        let open Unsigned.UInt32 in
        f (handle_of_opt v)
          (of_int time) modifiers typ
          (of_int slot) origin;
  }

  let c_of_it it =
    let c_it = make C.Interface_Touch.touch in
    setf c_it C.Interface_Touch.touchf
      (fun v time modifiers typ slot origin ->
         let open Unsigned.UInt32 in
         it.touch (opt_of_handle v)
           (to_int time) modifiers typ
           (to_int slot) origin);
    addr c_it

  let touch : touch typ = Ctypes.view
      ~read:it_of_c
      ~write:c_of_it
      (ptr C.Interface_Touch.touch)

  type t = {
    view     : view;
    keyboard : keyboard;
    pointer  : pointer;
    output   : output;
    touch    : touch;
  }

  let t_of_c c_int = {
    output = c_int |-> C.output |> io_of_c;
    view = c_int |-> C.view |> iv_of_c;
    keyboard = c_int |-> C.keyboard |> ik_of_c;
    pointer = c_int |-> C.pointer |> ip_of_c;
    touch = c_int |-> C.touch |> it_of_c;
  }

  let c_of_t int =
    let c_int = make C.interface in
    setf c_int C.output (!@ (c_of_io int.output));
    setf c_int C.view (!@ (c_of_iv int.view));
    setf c_int C.keyboard (!@ (c_of_ik int.keyboard));
    setf c_int C.pointer (!@ (c_of_ip int.pointer));
    setf c_int C.touch (!@ (c_of_it int.touch));
    addr c_int

  let t : t typ = Ctypes.view
      ~read:t_of_c
      ~write:c_of_t
      (ptr C.interface)
end


let init inter =
  foreign ~from:wlc_lib "wlc_init"
    (Interface.t @-> int @-> ptr string @-> returning bool)
    inter
    (Array.length Sys.argv)
    (CArray.of_list string (Array.to_list Sys.argv) |> CArray.start)
  |> fun b -> if not b then raise Failure else ()

let terminate = foreign ~from:wlc_lib "wlc_terminate"
    (void @-> returning void)

let run = foreign ~from:wlc_lib "wlc_run"
    (void @-> returning void)

(* log stuff (TODO) *)
let log msg =
  foreign ~from:wlc_lib "wlc_log"
    (uint32_t @-> string @-> string @-> returning void)
    Unsigned.UInt32.zero
    "%s"
    msg
