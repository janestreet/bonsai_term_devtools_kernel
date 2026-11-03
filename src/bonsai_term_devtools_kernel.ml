open! Core
open Bonsai_term
open Bonsai.Let_syntax
open Event
module Diagnose = Bonsai_term_devtools_diagnose
module Tile_layout = Bonsai_term_tile_layout

module Colors = struct
  let bg ~flavor = Bonsai_term_color_scheme.color ~flavor Bonsai_term_color_scheme.Crust
  let fg ~flavor = Bonsai_term_color_scheme.color ~flavor Bonsai_term_color_scheme.Text
end

module Key_binding = struct
  type t =
    { shortcut : Bonsai_term_shortcut.t
    ; display : string option
    }

  let create ?mods ?display key =
    { shortcut = Bonsai_term_shortcut.create ?mods key; display }
  ;;

  let matches t event = Bonsai_term_shortcut.matches t.shortcut event

  let display t =
    match t.display with
    | Some display -> display
    | None -> Bonsai_term_shortcut.to_string_hum ~style:Verbose t.shortcut
  ;;
end

module Key_bindings = struct
  type t =
    { toggle : Key_binding.t list
    ; focus_forward : Key_binding.t list
    ; focus_backward : Key_binding.t list
    }

  let default =
    { toggle = [ Key_binding.create (Function 12) ]
    ; focus_forward = [ Key_binding.create ~mods:[ Ctrl ] (Function 12) ]
    ; focus_backward = [ Key_binding.create ~mods:[ Ctrl; Shift ] (Function 12) ]
    }
  ;;

  let none = { toggle = []; focus_forward = []; focus_backward = [] }

  let create
    ?(toggle = default.toggle)
    ?(focus_forward = default.focus_forward)
    ?(focus_backward = default.focus_backward)
    ()
    =
    { toggle; focus_forward; focus_backward }
  ;;

  let matches bindings event =
    List.exists bindings ~f:(fun key_binding -> Key_binding.matches key_binding event)
  ;;

  let hint bindings =
    bindings
    |> List.map ~f:(fun key_binding -> [%string "[%{Key_binding.display key_binding}]"])
    |> String.concat ~sep:"/"
  ;;
end

module Controls = struct
  module Value = struct
    type t =
      { is_open : bool
      ; open_ : unit Effect.t
      ; close : unit Effect.t
      ; toggle : unit Effect.t
      }
  end

  type t = Value.t Bonsai.Dynamic_scope.t

  let create () =
    let unavailable operation =
      Effect.of_thunk (fun () ->
        raise_s
          [%message
            "Devtools controls are unavailable; pass this handle as [~devtools_controls]"
              (operation : string)])
    in
    Bonsai.Dynamic_scope.create
      ~name:"bonsai-term-devtools-controls"
      ~fallback:
        { Value.is_open = false
        ; open_ = unavailable "open"
        ; close = unavailable "close"
        ; toggle = unavailable "toggle"
        }
      ()
  ;;

  let current = Bonsai.Dynamic_scope.lookup

  let is_open t graph =
    let%arr current = current t graph in
    current.Value.is_open
  ;;

  let open_ t graph =
    let%arr current = current t graph in
    current.Value.open_
  ;;

  let close t graph =
    let%arr current = current t graph in
    current.Value.close
  ;;

  let toggle t graph =
    let%arr current = current t graph in
    current.Value.toggle
  ;;

  let set t ~is_open ~open_ ~close ~toggle ~inside graph =
    let value =
      let%arr is_open and open_ and close and toggle in
      { Value.is_open; open_; close; toggle }
    in
    Bonsai.Dynamic_scope.set t value ~inside graph
  ;;
end

module Restart_env = struct
  let restarted_var = "BONSAI_TERM_DEVTOOLS_RESTARTED"

  let started_by_restart =
    lazy
      (let started_by_restart = Option.is_some (Sys.getenv restarted_var) in
       if started_by_restart
       then
         (* Safe: this runs once during app startup, before the app has had a chance to
            read the environment from anywhere else. Clearing the variable prevents
            subprocesses from incorrectly treating themselves as restarted apps. *)
         (Core_unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) restarted_var;
       started_by_restart)
  ;;

  let started_by_restart () = force started_by_restart

  let mark_restarted extend_env =
    let extend_env =
      List.filter extend_env ~f:(fun (name, _) -> not (String.equal name restarted_var))
    in
    (restarted_var, "1") :: extend_env
  ;;
end

module Exit_or_restart = struct
  type 'exit t =
    | Exit of 'exit
    | Restart of { extend_env : (string * string) list }
end

let restart_process ~extend_env =
  let argv = Sys.get_argv () |> Array.to_list in
  let extend_env = Restart_env.mark_restarted extend_env in
  never_returns
    (Core_unix.exec ~prog:(List.hd_exn argv) ~argv ~env:(`Extend extend_env) ())
;;

let crop_to_width ~width view =
  let overflow = View.width view - width in
  if overflow > 0 then View.crop ~r:overflow view else view
;;

let fill_row ~width ~attrs view =
  View.zcat [ crop_to_width ~width view; View.rectangle ~attrs ~width ~height:1 () ]
;;

let header_label = " Devtools "

let header (key_bindings : Key_bindings.t) =
  let toggle_hint = Key_bindings.hint key_bindings.toggle in
  let focus_hints =
    [ Key_bindings.hint key_bindings.focus_forward
    ; Key_bindings.hint key_bindings.focus_backward
    ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:"/"
  in
  let hints =
    [ Option.some_if (not (String.is_empty toggle_hint)) [%string "%{toggle_hint} close"]
    ; Option.some_if
        (not (String.is_empty focus_hints))
        [%string "%{focus_hints} cycle focus"]
    ]
    |> List.filter_opt
  in
  let header_lines =
    match hints with
    | [] -> [ header_label ]
    | hints -> [ header_label; [%string " %{String.concat hints ~sep:\"  \"} "] ]
  in
  Tile_layout.tile
    ~key:"devtools-header"
    ~initial_size:(Cells (List.length header_lines))
    ~focusable:false
    ~resizable:false
    (fun ~dimensions ~is_focused:_ (graph @ local) ->
      let flavor = Bonsai_term_color_scheme.flavor graph in
      let%arr { Dimensions.width; _ } = dimensions
      and flavor in
      let attrs = [ Attr.fg (Colors.fg ~flavor); Attr.bg (Colors.bg ~flavor) ] in
      let view =
        header_lines
        |> List.mapi ~f:(fun index line ->
          let attrs = if index = 0 then Attr.bold :: attrs else attrs in
          View.text ~attrs line |> fill_row ~width ~attrs:[ Attr.bg (Colors.bg ~flavor) ])
        |> View.vcat
      in
      ~view, ~handler:(fun _ -> Captured_or_ignored.ignore))
;;

let restart_panel ~request_restart =
  Tile_layout.tile
    ~key:"restart-panel"
    ~initial_size:(Cells 3)
    (fun ~dimensions ~is_focused (graph @ local) ->
       let flavor = Bonsai_term_color_scheme.flavor graph in
       let command_line = Sys.get_argv () |> Array.to_list |> String.concat ~sep:" " in
       let%arr { Dimensions.width; height } = dimensions
       and flavor
       and is_focused in
       let text
         ?(attrs = [])
         ?(fg : Bonsai_term_color_scheme.t = Bonsai_term_color_scheme.Text)
         string
         =
         View.text
           ~attrs:
             ([ Attr.fg (Bonsai_term_color_scheme.color ~flavor fg)
              ; Attr.bg (Colors.bg ~flavor)
              ]
              @ attrs)
           string
       in
       let indicator = if is_focused then "▶" else " " in
       let title =
         text
           ~attrs:(if is_focused then [ Attr.bold ] else [])
           [%string " %{indicator} Restart "]
       in
       let hint =
         if is_focused
         then " [Enter] restart this app: "
         else " Focus + [Enter] to restart this app: "
       in
       let view =
         View.vcat
           [ title
           ; text ~fg:Overlay2 ~attrs:[ Attr.italic ] hint
           ; text ~attrs:[ Attr.bold ] [%string " $ %{command_line}"]
           ]
       in
       let backdrop =
         View.rectangle ~attrs:[ Attr.bg (Colors.bg ~flavor) ] ~width ~height ()
       in
       let handler (event : Event.t) =
         match event with
         | Key_press { key = Enter; mods = [] } ->
           Captured_or_ignored.capture request_restart
         | _ -> Captured_or_ignored.ignore
       in
       ~view:(View.zcat [ view; backdrop ]), ~handler)
;;

let with_devtools
  ?max_console_messages
  ?start_open
  ?request_restart
  ?devtools_controls
  ?(devtools_key_bindings = Key_bindings.default)
  ?(capture_global_log = true)
  ?(additional_panels = [])
  ~dimensions
  app
  (graph @ local)
  =
  Expert.Console_debug.enable_console ?max_console_messages ();
  if capture_global_log then Expert.Console_debug.capture_global_log ();
  let started_by_restart = Restart_env.started_by_restart () in
  let start_open = Option.value start_open ~default:false || started_by_restart in
  let { Bonsai.Toggle.state = is_devtools_open; set_state = set_devtools_open; toggle } =
    Bonsai.toggle' ~default_model:start_open graph
  in
  let open_ =
    let%arr set_devtools_open in
    set_devtools_open true
  in
  let close =
    let%arr set_devtools_open in
    set_devtools_open false
  in
  let devtools_controls =
    Option.value_or_thunk devtools_controls ~default:Controls.create
  in
  let diagnose = Diagnose.create graph in
  let record_event = Diagnose.record_event diagnose in
  let content =
    Tile_layout.tile
      ~key:"content"
      ~initial_size:(Weight 3)
      (fun ~dimensions ~is_focused:_ (graph @ local) ->
         let view_and_handler =
           Controls.set
             devtools_controls
             ~is_open:is_devtools_open
             ~open_
             ~close
             ~toggle
             ~inside:(fun graph ->
               let ~view, ~handler = app ~dimensions graph in
               Bonsai.both view handler)
             graph
         in
         let%arr view, handler = view_and_handler
         and is_devtools_open
         and record_event in
         let handler event =
           let%bind.Effect result = handler event in
           let%map.Effect () =
             if is_devtools_open
             then
               record_event
                 { Diagnose.Event_entry.event
                 ; attribution = Handled_by_app (Captured_or_ignored.positions result)
                 }
             else Effect.Ignore
           in
           result
         in
         ~view, ~handler)
  in
  let diagnose_panel = Diagnose.panel diagnose ~app:"content" in
  let restart_panels =
    Option.to_list
      (Option.map request_restart ~f:(fun request_restart ->
         restart_panel ~request_restart))
  in
  let panels =
    [ diagnose_panel; Console_panel.panel () ] @ restart_panels @ additional_panels
  in
  let devtools =
    Tile_layout.container
      ~key:"devtools"
      ~initial_size:(Weight 2)
      ~orientation:Vertical
      (Nonempty_list.create (header devtools_key_bindings) panels)
  in
  let layout =
    Tile_layout.container
      ~key:"root"
      ~orientation:Horizontal
      (Nonempty_list.create content [ devtools ])
  in
  let is_narrow =
    let%arr { Dimensions.width; _ } = dimensions in
    (* Somewhat arbitrarily selected width, but big enough to show "Devtools" header and
       beginning of keyboard hints *)
    width < 25
  in
  let focus = Tile_layout.Focus.create ~initial:"content" graph in
  let content_is_focused =
    let%arr focused = Tile_layout.focused focus in
    Option.exists focused ~f:(Tile_layout.Key.equal "content")
  in
  let root =
    let%arr content_is_focused and is_devtools_open and is_narrow in
    match is_devtools_open, is_narrow with
    | false, _ -> "content"
    | true, false -> "root"
    | true, true ->
      (* On narrow terminals there is no room for a split, so show whichever pane holds
         focus at the full dimensions. *)
      if content_is_focused then "content" else "devtools"
  in
  let divider_attrs =
    let flavor = Bonsai_term_color_scheme.flavor graph in
    let%arr flavor in
    [ Attr.fg (Colors.fg ~flavor); Attr.bg (Colors.bg ~flavor) ]
  in
  let output =
    Tile_layout.component
      ~focus:(`Handle focus)
      ~root
      ~divider_attrs
      ~dimensions
      ~layout
      graph
  in
  (* Handle focus on narrow terminal displays. *)
  Bonsai.Edge.on_change'
    ~trigger:`Before_display
    ~equal:Bool.equal
    is_devtools_open
    ~callback:
      (let%arr is_narrow and output in
       fun previous is_open ->
         match previous with
         | None ->
           if is_open && is_narrow
           then output.actions.focus (Some "diagnose-panel")
           else Effect.Ignore
         | Some _ ->
           output.actions.focus
             (Some (if is_open && is_narrow then "diagnose-panel" else "content")))
    graph;
  (* If the focus moves to content in a narrow terminal with devtools open, close the
     devtools. *)
  Bonsai.Edge.on_change'
    ~trigger:`Before_display
    ~equal:Bool.equal
    content_is_focused
    ~callback:
      (let%arr is_narrow and is_devtools_open and set_devtools_open in
       fun previous content_is_focused ->
         match previous, content_is_focused with
         | Some false, true when is_narrow && is_devtools_open -> set_devtools_open false
         | _ -> Effect.Ignore)
    graph;
  let view =
    let%arr output in
    output.view
  in
  let handler =
    let%arr is_devtools_open and output and toggle in
    fun (event : Event.t) ->
      if Key_bindings.matches devtools_key_bindings.toggle event
      then Captured_or_ignored.capture toggle
      else if is_devtools_open
              && Key_bindings.matches devtools_key_bindings.focus_forward event
      then
        Captured_or_ignored.capture
          (output.actions.move_focus ~within:None Tile_layout.Focus_direction.Forward)
      else if is_devtools_open
              && Key_bindings.matches devtools_key_bindings.focus_backward event
      then
        Captured_or_ignored.capture
          (output.actions.move_focus ~within:None Tile_layout.Focus_direction.Backward)
      else output.handler ~keyboard_fallback:"content" event
  in
  ~view, ~handler
;;

let make_app_exit_on_ctrlc app =
  let app ~exit ~dimensions graph =
    let ~view, ~handler = app ~exit ~dimensions graph in
    let handler =
      let%arr.Bonsai handler in
      Default_exit_handler.ctrl_c_handler_wrapper
        ~on_ctrl_c:(fun () ->
          Captured_or_ignored.capture (exit (Exit_or_restart.Exit ())))
        ~base_handler:handler
    in
    ~view, ~handler
  in
  app
;;

let start_with_exit_impl
  ?max_console_messages
  ?start_open
  ?prepare_to_restart
  ?devtools_controls
  ?devtools_key_bindings
  ?capture_global_log
  ?additional_panels
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  ?instrumentation
  ~wrap_app
  app
  =
  let open Async.Deferred.Or_error.Let_syntax in
  let app_with_devtools ~exit ~dimensions graph =
    let request_restart =
      let%bind.Effect () = Option.value prepare_to_restart ~default:Effect.Ignore in
      exit (Exit_or_restart.Restart { extend_env = [] })
    in
    let ~view, ~handler =
      with_devtools
        ?max_console_messages
        ?start_open
        ~request_restart
        ?devtools_controls
        ?devtools_key_bindings
        ?capture_global_log
        ?additional_panels
        ~dimensions
        (app ~exit:(fun result -> exit (Exit_or_restart.Exit result)))
        graph
    in
    ~view, ~handler
  in
  let app_with_devtools = wrap_app app_with_devtools in
  let app_with_unit_handler ~exit ~dimensions graph =
    let ~view, ~handler = app_with_devtools ~exit ~dimensions graph in
    let handler =
      let%arr.Bonsai handler in
      fun event -> Effect.ignore_m (handler event)
    in
    ~view, ~handler
  in
  match%bind
    Bonsai_term.start_with_exit
      ?dispose
      ?nosig
      ?mouse
      ?bpaste
      ?reader
      ?writer
      ?time_source
      ?optimize
      ?target_frames_per_second
      ?for_mocking
      ?instrumentation
      app_with_unit_handler
  with
  | Exit_or_restart.Exit result -> return result
  | Exit_or_restart.Restart { extend_env } -> restart_process ~extend_env
;;

let start_with_exit = start_with_exit_impl ~wrap_app:Fn.id

let start
  ?max_console_messages
  ?start_open
  ?prepare_to_restart
  ?devtools_controls
  ?devtools_key_bindings
  ?capture_global_log
  ?additional_panels
  ?dispose
  ?nosig
  ?mouse
  ?bpaste
  ?reader
  ?writer
  ?time_source
  ?optimize
  ?target_frames_per_second
  ?for_mocking
  ?instrumentation
  app
  =
  start_with_exit_impl
    ?max_console_messages
    ?start_open
    ?prepare_to_restart
    ?devtools_controls
    ?devtools_key_bindings
    ?capture_global_log
    ?additional_panels
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?reader
    ?writer
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?for_mocking
    ?instrumentation
    ~wrap_app:make_app_exit_on_ctrlc
    (fun ~exit:_ ~dimensions graph -> app ~dimensions graph)
;;

(* Confirm that [start]'s and [start_with_exit]'s arguments are kept in sync with their
   [Bonsai_term] counterparts.

   [module type of] infers the current types of [Bonsai_term.start] and
   [Bonsai_term.start_with_exit]. The anonymous module [module _] must match those types
   after its devtools-specific arguments are fixed to [None] and its unit handlers are
   lifted to [Captured_or_ignored.t], so changing a shared argument without updating these
   wrappers will cause a compiler error. *)
module _ : module type of struct
  (* Suppress [unused-value-declaration] warnings because this structure is inspected only
     for its inferred signature, so its values are intentionally unused. *)
  [@@@warning "-32"]

  let start = Bonsai_term.start
  let start_with_exit = Bonsai_term.start_with_exit
end = struct
  let capture_handler app ~dimensions graph =
    let ~view, ~handler = app ~dimensions graph in
    let handler =
      let%arr.Bonsai handler in
      fun event -> Captured_or_ignored.capture (handler event)
    in
    ~view, ~handler
  ;;

  let start
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?reader
    ?writer
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?for_mocking
    ?instrumentation
    app
    =
    start
      ?max_console_messages:None
      ?start_open:None
      ?prepare_to_restart:None
      ?devtools_controls:None
      ?devtools_key_bindings:None
      ?capture_global_log:None
      ?additional_panels:None
      ?dispose
      ?nosig
      ?mouse
      ?bpaste
      ?reader
      ?writer
      ?time_source
      ?optimize
      ?target_frames_per_second
      ?for_mocking
      ?instrumentation
      (capture_handler app)
  ;;

  let start_with_exit
    ?dispose
    ?nosig
    ?mouse
    ?bpaste
    ?reader
    ?writer
    ?time_source
    ?optimize
    ?target_frames_per_second
    ?for_mocking
    ?instrumentation
    app
    =
    start_with_exit
      ?max_console_messages:None
      ?start_open:None
      ?prepare_to_restart:None
      ?devtools_controls:None
      ?devtools_key_bindings:None
      ?capture_global_log:None
      ?additional_panels:None
      ?dispose
      ?nosig
      ?mouse
      ?bpaste
      ?reader
      ?writer
      ?time_source
      ?optimize
      ?target_frames_per_second
      ?for_mocking
      ?instrumentation
      (fun ~exit ~dimensions graph -> capture_handler (app ~exit) ~dimensions graph)
  ;;
end

module Expert = struct
  let with_devtools = with_devtools
end

module For_testing = struct
  let with_devtools = with_devtools
end
