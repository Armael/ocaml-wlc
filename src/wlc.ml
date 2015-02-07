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

let ptr_of_opt ty = function
  | Some p -> p
  | None -> coerce (ptr void) ty null

let opt_of_ptr ty p =
  if ptr_eq (coerce ty (ptr void) p) null then None
  else Some p

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

module Space = struct
  module C = struct
    type space
    let space : space structure typ = structure "wlc_space"
  end

  type t = C.space structure ptr
  let t = ptr C.space

  (* for internal use only *)
  let get_link = foreign ~from:wlc_lib "wlc_space_get_link"
      (t @-> returning Wl.wl_list)

  (* for internal use only *)
  let from_link = foreign ~from:wlc_lib "wlc_space_from_link"
      (Wl.wl_list @-> returning t)

  let is_single space =
    let l = get_link space in
    ptr_eq (Wl.prev l) (Wl.next l)

  let remove = foreign ~from:wlc_lib "wlc_space_remove"
      (t @-> returning void)
end

module Output = struct
  module C = struct
    type output
    let output : output structure typ = structure "wlc_output"
  end

  type t = C.output structure ptr
  let t = ptr C.output

  let get_pixels output async =
    foreign ~from:wlc_lib "wlc_output_get_pixels"
      (t @-> (funptr (size @-> ptr uint8_t @-> returning void))
         @-> returning void)
      output
      (fun s buf ->
         let buf = coerce (ptr uint8_t) (ptr int) buf in
         async (bigarray_of_ptr array2 (s.w, s.h) Bigarray.int8_unsigned buf))

  let set_resolution = foreign ~from:wlc_lib "wlc_output_set_resolution"
      (t @-> size @-> returning void)

  let get_resolution = foreign ~from:wlc_lib "wlc_output_get_resolution"
      (t @-> returning size)

  let get_active_space = foreign ~from:wlc_lib "wlc_output_get_active_space"
      (t @-> returning Space.t)

  (* for internal use only *)
  let get_link = foreign ~from:wlc_lib "wlc_output_get_link"
      (t @-> returning Wl.wl_list)

  (* for internal use only *)
  let from_link = foreign ~from:wlc_lib "wlc_output_from_link"
      (Wl.wl_list @-> returning t)

  let is_single output =
    let l = get_link output in
    ptr_eq (Wl.prev l) (Wl.next l)

  let get_spaces output =
    foreign ~from:wlc_lib "wlc_output_get_spaces"
      (t @-> returning Wl.wl_list)
      output
    |> Wl.ocaml_of_wl_list Space.from_link

  let focus_space = foreign ~from:wlc_lib "wlc_output_focus_space"
      (t @-> Space.t @-> returning void)

  let of_space = foreign ~from:wlc_lib "wlc_space_get_output"
      (Space.t @-> returning t)

  let add_space output =
    foreign ~from:wlc_lib "wlc_space_add"
      (t @-> returning Space.t)
      output
    |> fail_if_null Space.t
end

module View = struct
  module C = struct
    type view
    let view : view structure typ = structure "wlc_view"
  end

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

  type t = C.view structure ptr
  let t = ptr C.view

  (* for internal use only *)
  let get_link = foreign ~from:wlc_lib "wlc_view_get_link"
      (t @-> returning Wl.wl_list)

  (* for internal use only *)
  let from_link = foreign ~from:wlc_lib "wlc_view_from_link"
      (Wl.wl_list @-> returning t)

  let is_single view =
    let l = get_link view in
    ptr_eq (Wl.prev l) (Wl.next l)

  let set_title = foreign ~from:wlc_lib "wlc_view_set_title"
      (t @-> string @-> returning void)

  let get_title = foreign ~from:wlc_lib "wlc_view_get_title"
      (t @-> returning string)

  let set_class = foreign ~from:wlc_lib "wlc_view_set_class"
      (t @-> string_opt @-> returning void)

  let get_class = foreign ~from:wlc_lib "wlc_view_get_class"
      (t @-> returning string_opt)

  let set_space = foreign ~from:wlc_lib "wlc_view_set_space"
      (t @-> Space.t @-> returning void)

  let get_space = foreign ~from:wlc_lib "wlc_view_get_space"
      (t @-> returning Space.t)

  let all_of_space space =
    foreign ~from:wlc_lib "wlc_space_get_views"
      (Space.t @-> returning Wl.wl_list)
      space
    |> Wl.ocaml_of_wl_list from_link

  let get_type = foreign ~from:wlc_lib "wlc_view_get_type"
      (t @-> returning typ)

  let get_state = foreign ~from:wlc_lib "wlc_view_get_state"
      (t @-> returning state)

  let set_state = foreign ~from:wlc_lib "wlc_view_set_state"
      (t @-> state_bit @-> bool @-> returning void)

  let get_geometry = foreign ~from:wlc_lib "wlc_view_get_geometry"
      (t @-> returning geometry)

  let set_geometry = foreign ~from:wlc_lib "wlc_view_set_geometry"
      (t @-> geometry @-> returning void)

  let close = foreign ~from:wlc_lib "wlc_view_close"
      (t @-> returning void)

  let send_below = foreign ~from:wlc_lib "wlc_view_send_below"
      (t @-> t @-> returning void)

  let send_to_back = foreign ~from:wlc_lib "wlc_view_send_to_back"
      (t @-> returning void)

  let bring_above = foreign ~from:wlc_lib "wlc_view_bring_above"
      (t @-> t @-> returning void)

  let bring_to_front = foreign ~from:wlc_lib "wlc_view_bring_to_front"
      (t @-> returning void)

  let set_parent view parent =
    foreign ~from:wlc_lib "wlc_view_set_parent"
      (t @-> t @-> returning void)
      view
      (ptr_of_opt t parent)

  let get_parent view =
    foreign ~from:wlc_lib "wlc_view_get_parent"
      (t @-> returning t)
      view
    |> opt_of_ptr t
end

module Compositor = struct
  module C = struct
    type compositor
    let compositor : compositor structure typ = structure "wlc_compositor"
  end

  type t = C.compositor structure ptr
  let t = ptr C.compositor

  let get_outputs comp =
    foreign ~from:wlc_lib "wlc_compositor_get_outputs"
      (t @-> returning Wl.wl_list)
      comp
    |> Wl.ocaml_of_wl_list Output.from_link

  let get_focused_output =
    foreign ~from:wlc_lib "wlc_compositor_get_focused_output"
      (t @-> returning Output.t)

  let get_focused_space =
    foreign ~from:wlc_lib "wlc_compositor_get_focused_space"
      (t @-> returning Space.t)

  let focus_view comp view =
    foreign ~from:wlc_lib "wlc_compositor_focus_view"
      (t @-> View.t @-> returning void)
      comp
      (ptr_of_opt View.t view)

  let focus_output = foreign ~from:wlc_lib "wlc_compositor_focus_output"
      (t @-> Output.t @-> returning void)

  let create () =
    foreign ~from:wlc_lib "wlc_compositor_new"
      ((ptr void) @-> returning t)
      null
    |> fail_if_null t
end

module Interface = struct
  module C = struct
    module Interface_View = struct
      module Request = struct
        type request
        let request : request structure typ = structure "request"
        let geometry = field request "geometry"
            (funptr (Compositor.t @-> View.t
                     @-> geometry
                     @-> returning void))
        let state = field request "state"
            (funptr (Compositor.t @-> View.t @-> View.state_bit @-> bool
                     @-> returning void))
        let () = seal request
      end

      type view
      let view : view structure typ = structure "view"
      let created = field view "created"
          (funptr (Compositor.t @-> View.t @-> Space.t @-> returning bool))
      let destroyed = field view "destroyed"
          (funptr (Compositor.t @-> View.t @-> returning void))
      let switch_space = field view "switch_space"
          (funptr (Compositor.t @-> View.t @-> Space.t @-> Space.t
                   @-> returning void))
      let request = field view "request" Request.request
      let () = seal view
    end

    module Interface_Keyboard = struct
      type keyboard
      let keyboard : keyboard structure typ = structure "keyboard"
      let key = field keyboard "key"
          (funptr (Compositor.t @-> View.t @-> uint32_t
                   @-> modifiers @-> uint32_t @-> uint32_t
                   @-> key_state @-> returning bool))
      let () = seal keyboard
    end

    module Interface_Pointer = struct
      type pointer
      let pointer : pointer structure typ = structure "pointer"
      let button = field pointer "button"
          (funptr (Compositor.t @-> View.t @-> uint32_t
                   @-> modifiers @-> uint32_t
                   @-> button_state @-> returning bool))
      let scroll = field pointer "scroll"
          (funptr (Compositor.t @-> View.t @-> uint32_t
                   @-> modifiers @-> scroll_axis
                   @-> ptr double @-> returning bool))
      let motion = field pointer "motion"
          (funptr (Compositor.t @-> View.t @-> uint32_t
                   @-> origin @-> returning bool))
      let () = seal pointer
    end

    module Interface_Output = struct
      type output
      let output : output structure typ = structure "output"
      let created = field output "created"
          (funptr (Compositor.t @-> Output.t @-> returning bool))
      let destroyed = field output "destroyed"
          (funptr (Compositor.t @-> Output.t @-> returning void))
      let activated = field output "activated"
          (funptr (Compositor.t @-> Output.t @-> returning void))
      let resolution = field output "resolution"
          (funptr (Compositor.t @-> Output.t @-> size
                   @-> returning void))
      let () = seal output
    end

    module Interface_Space = struct
      type space
      let space : space structure typ = structure "space"
      let created = field space "created"
          (funptr (Compositor.t @-> Space.t @-> returning bool))
      let destroyed = field space "destroyed"
          (funptr (Compositor.t @-> Space.t @-> returning void))
      let activated = field space "activated"
          (funptr (Compositor.t @-> Space.t @-> returning void))
      let () = seal space
    end

    type interface
    let interface : interface structure typ = structure "wlc_interface"
    let view = field interface "view" Interface_View.view
    let keyboard = field interface "keyboard" Interface_Keyboard.keyboard
    let pointer = field interface "pointer" Interface_Pointer.pointer
    let output = field interface "output" Interface_Output.output
    let space = field interface "space" Interface_Space.space
    let () = seal interface
  end

  type view_request = {
    geometry : Compositor.t -> View.t -> geometry -> unit;
    state    : Compositor.t -> View.t -> View.state_bit -> bool -> unit;
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
    created      : Compositor.t -> View.t -> Space.t -> bool;
    destroyed    : Compositor.t -> View.t -> unit;
    switch_space : Compositor.t -> View.t -> Space.t -> Space.t -> unit;

    request      : view_request;
  }

  let iv_of_c c_iv = {
    created = getf (!@ c_iv) C.Interface_View.created;
    destroyed = getf (!@ c_iv) C.Interface_View.destroyed;
    switch_space = getf (!@ c_iv) C.Interface_View.switch_space;
    request = c_iv |-> C.Interface_View.request |> ivr_of_c;
  }

  let c_of_iv iv =
    let c_iv = make C.Interface_View.view in
    setf c_iv C.Interface_View.created iv.created;
    setf c_iv C.Interface_View.destroyed iv.destroyed;
    setf c_iv C.Interface_View.switch_space iv.switch_space;
    setf c_iv C.Interface_View.request (!@ (c_of_ivr iv.request));
    addr c_iv

  let view : view typ = view
      ~read:iv_of_c
      ~write:c_of_iv
      (ptr C.Interface_View.view)

  type keyboard = {
    key : Compositor.t ->
      View.t -> int -> modifiers -> int -> Keysym.t -> key_state -> bool;
  }

  let ik_of_c c_ik = {
    key =
      let f = getf (!@ c_ik) C.Interface_Keyboard.key in
      fun c v time modifiers key sym st ->
        let open Unsigned.UInt32 in
        f c v
          (of_int time) modifiers
          (of_int key) (of_int sym)
          st;
  }

  let c_of_ik ik =
    let c_ik = make C.Interface_Keyboard.keyboard in
    setf c_ik C.Interface_Keyboard.key
      (fun c v time modifiers key sym st ->
         let open Unsigned.UInt32 in
         ik.key c v
           (to_int time) modifiers
           (to_int key) (to_int sym)
           st);
    addr c_ik

  let keyboard : keyboard typ = Ctypes.view
      ~read:ik_of_c
      ~write:c_of_ik
      (ptr C.Interface_Keyboard.keyboard)

  type pointer = {
    button : Compositor.t -> View.t -> int -> modifiers -> int -> button_state -> bool;
    scroll : Compositor.t -> View.t -> int -> modifiers -> scroll_axis_bit list ->
      float * float -> bool;
    motion : Compositor.t -> View.t -> int -> origin -> bool;
  }

  let ip_of_c c_ip =
    let open Unsigned.UInt32 in
    let c_button = getf (!@ c_ip) C.Interface_Pointer.button in
    let c_scroll = getf (!@ c_ip) C.Interface_Pointer.scroll in
    let c_motion = getf (!@ c_ip) C.Interface_Pointer.motion in
    {
      button = (fun c v time modifiers button st ->
        c_button c v (of_int time) modifiers (of_int button) st);
      scroll = (fun c v time modifiers axis (a, b) ->
        let amount = CArray.of_list double [a; b] |> CArray.start in
        c_scroll c v (of_int time) modifiers axis amount);
      motion = (fun c v time origin ->
        c_motion c v (of_int time) origin);
    }

  let c_of_ip ip =
    let open Unsigned.UInt32 in
    let c_ip = make C.Interface_Pointer.pointer in
    setf c_ip C.Interface_Pointer.button
      (fun c v time modifiers button st ->
         ip.button c v
           (to_int time) modifiers
           (to_int button)
           st);
    setf c_ip C.Interface_Pointer.scroll
      (fun c v time modifiers axis amount ->
         let arr = CArray.from_ptr amount 2 in
         ip.scroll c v
           (to_int time) modifiers axis
           (CArray.get arr 0, CArray.get arr 1));
    setf c_ip C.Interface_Pointer.motion
      (fun c v time origin -> ip.motion c v (to_int time) origin);
    addr c_ip

  let pointer : pointer typ = Ctypes.view
      ~read:ip_of_c
      ~write:c_of_ip
      (ptr C.Interface_Pointer.pointer)

  type output = {
    created    : Compositor.t -> Output.t -> bool;
    destroyed  : Compositor.t -> Output.t -> unit;
    activated  : Compositor.t -> Output.t -> unit;
    resolution : Compositor.t -> Output.t -> size -> unit;
  }

  let io_of_c c_io = {
    created = getf (!@ c_io) C.Interface_Output.created;
    destroyed = getf (!@ c_io) C.Interface_Output.destroyed;
    activated = getf (!@ c_io) C.Interface_Output.activated;
    resolution = getf (!@ c_io) C.Interface_Output.resolution;
  }

  let c_of_io io =
    let c_io = make C.Interface_Output.output in
    setf c_io C.Interface_Output.created io.created;
    setf c_io C.Interface_Output.destroyed io.destroyed;
    setf c_io C.Interface_Output.activated io.activated;
    setf c_io C.Interface_Output.resolution io.resolution;
    addr c_io

  let output : output typ = Ctypes.view
      ~read:io_of_c
      ~write:c_of_io
      (ptr C.Interface_Output.output)

  type space = {
    created    : Compositor.t -> Space.t -> bool;
    destroyed  : Compositor.t -> Space.t -> unit;
    activated  : Compositor.t -> Space.t -> unit;
  }

  let is_of_c c_is = {
    created = getf (!@ c_is) C.Interface_Space.created;
    destroyed = getf (!@ c_is) C.Interface_Space.destroyed;
    activated = getf (!@ c_is) C.Interface_Space.activated;
  }

  let c_of_is is =
    let c_is = make C.Interface_Space.space in
    setf c_is C.Interface_Space.created is.created;
    setf c_is C.Interface_Space.destroyed is.destroyed;
    setf c_is C.Interface_Space.activated is.activated;
    addr c_is

  let space : space typ = Ctypes.view
      ~read:is_of_c
      ~write:c_of_is
      (ptr C.Interface_Space.space)

  type t = {
    view     : view;
    keyboard : keyboard;
    pointer  : pointer;
    output   : output;
    space    : space;
  }

  let t_of_c c_int = {
    view = c_int |-> C.view |> iv_of_c;
    keyboard = c_int |-> C.keyboard |> ik_of_c;
    pointer = c_int |-> C.pointer |> ip_of_c;
    output = c_int |-> C.output |> io_of_c;
    space = c_int |-> C.space |> is_of_c;
  }

  let c_of_t int =
    let c_int = make C.interface in
    setf c_int C.view (!@ (c_of_iv int.view));
    setf c_int C.keyboard (!@ (c_of_ik int.keyboard));
    setf c_int C.pointer (!@ (c_of_ip int.pointer));
    setf c_int C.output (!@ (c_of_io int.output));
    setf c_int C.space (!@ (c_of_is int.space));
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

let terminate () = foreign ~from:wlc_lib "wlc_terminate"
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
