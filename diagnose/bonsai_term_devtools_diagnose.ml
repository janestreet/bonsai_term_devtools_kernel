open! Core
open Bonsai_term
open Bonsai.Let_syntax

let green ~flavor x =
  View.text
    ~attrs:
      [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
      ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Green)
      ; Attr.bold
      ]
    x
;;

let text
  ~flavor
  ?(attrs = [])
  ?(bg : Bonsai_term_color_scheme.t = Crust)
  ?(fg : Bonsai_term_color_scheme.t = Text)
  x
  =
  View.text
    ~attrs:
      ([ Attr.bg (Bonsai_term_color_scheme.color ~flavor bg)
       ; Attr.fg (Bonsai_term_color_scheme.color ~flavor fg)
       ]
       @ attrs)
    x
;;

let hcat_with_separator items =
  let sep = View.transparent_rectangle ~width:1 ~height:1 in
  View.hcat (List.intersperse ~sep items)
;;

let section ~flavor title = text ~flavor ~fg:Yellow title

let style_samples ~flavor =
  hcat_with_separator
    [ text ~flavor "Normal"
    ; text ~flavor ~attrs:[ Attr.bold ] "Bold"
    ; text ~flavor ~attrs:[ Attr.italic ] "Italic"
    ; text ~flavor ~attrs:[ Attr.bold; Attr.italic ] "Bold+Italic"
    ]
;;

let render_mod mods f name = if List.exists ~f mods then name ^ "+" else ""

let render_event ~flavor event =
  match event with
  | Event.Key_press { key; mods } ->
    let meta =
      render_mod
        mods
        (function
          | Meta -> true
          | _ -> false)
        "Meta"
    and ctrl =
      render_mod
        mods
        (function
          | Ctrl -> true
          | _ -> false)
        "Ctrl"
    and shift =
      render_mod
        mods
        (function
          | Shift -> true
          | _ -> false)
        "Shift"
    in
    let prefix = String.concat [ meta; ctrl; shift ] in
    let key =
      match key with
      | ASCII ' ' -> "SPACE"
      | ASCII c -> Char.to_string c
      | Uchar uchar -> Uchar.Utf8.to_string uchar
      | ( Escape
        | Enter
        | Tab
        | Backspace
        | Insert
        | Delete
        | Home
        | End
        | Arrow _
        | Page _
        | Function _ ) as event -> Sexp.to_string_mach [%sexp (event : Event.Key.t)]
    in
    View.hcat
      [ green ~flavor "Key "
      ; View.text
          ~attrs:
            [ Attr.bold
            ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Blue)
            ; Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
            ]
          (prefix ^ key)
      ]
  | Mouse { kind; position; mods } ->
    View.hcat
      [ green ~flavor "Mouse "
      ; View.text
          ~attrs:
            [ Attr.bold
            ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Blue)
            ; Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
            ]
          (Sexp.to_string
             [%message
               (kind : Event.mouse_kind)
                 (position : Position.t)
                 (mods : Event.Modifier.t list)])
      ]
  | Paste `Start -> View.hcat [ green ~flavor "Paste (start)" ]
  | Paste `End -> View.hcat [ green ~flavor "Paste (end)" ]
;;

let app ~(dimensions : Dimensions.t Bonsai.t) (graph @ local) =
  let events, add_event =
    Bonsai.state_machine
      ~default_model:[]
      ~apply_action:(fun _ events event -> event :: List.take events 99)
      graph
  in
  let flavor = Bonsai_term_color_scheme.flavor graph in
  let view =
    let%arr events
    and { height; width } = dimensions
    and flavor in
    let dimensions =
      let row name value =
        View.hcat
          [ View.text
              ~attrs:
                [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
                ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Yellow)
                ]
              [%string "%{name}: "]
          ; View.text
              ~attrs:
                [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
                ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Mauve)
                ; Attr.bold
                ]
              [%string "%{value#Int}"]
          ]
      in
      View.vcat [ row "Height" height; row "Width" width ]
    in
    let title =
      View.hcat
        [ View.text
            ~attrs:
              [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
              ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Mauve)
              ; Attr.bold
              ]
            "Bonsai Term"
        ; View.text
            ~attrs:
              [ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
              ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Overlay2)
              ]
            " (diagnose terminal environment info)"
        ]
    in
    let body =
      [ dimensions
      ; View.hcat
          [ section ~flavor "Text styles: "; View.pad ~l:1 (style_samples ~flavor) ]
      ; View.hcat
          [ section ~flavor "Links: "
          ; text
              ~flavor
              ~fg:Blue
              ~attrs:[ Attr.underline; Attr.href "https://example.com" ]
              "href-implicit"
          ; section ~flavor " explicit: "
          ; text ~flavor "https://example.com"
          ; section ~flavor " both: "
          ; text
              ~flavor
              ~fg:Blue
              ~attrs:[ Attr.underline; Attr.href "https://example.com" ]
              "https://example.com"
          ]
      ; section ~flavor "Events:"
      ]
      @ List.map events ~f:(fun event -> View.pad ~l:2 (render_event ~flavor event))
    in
    View.vcat [ title; View.text ""; View.pad ~l:2 (View.vcat body) ]
  in
  let view =
    let%arr view
    and { width; height } = dimensions
    and flavor in
    let mark_attrs ~hundred =
      [ Attr.bold
      ; Attr.bg (Bonsai_term_color_scheme.color ~flavor (if hundred then Red else Yellow))
      ; Attr.fg (Bonsai_term_color_scheme.color ~flavor Crust)
      ]
    in
    let ruler_marks =
      let render ~hundred i =
        let attrs ~hundred =
          match i with
          | 0 -> mark_attrs ~hundred
          | _ ->
            [ Attr.fg (Bonsai_term_color_scheme.color ~flavor Text)
            ; Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust)
            ]
        in
        View.text ~attrs:(attrs ~hundred) [%string "%{i#Int}"]
      in
      let row =
        View.hcat
        @@ List.init width ~f:(fun i ->
          let i = i + 1 in
          let hundred = i % 100 = 0 in
          let i = i % 10 in
          let n = render ~hundred i in
          let mark = View.text ~attrs:(mark_attrs ~hundred) "│" in
          match i with
          | 0 -> View.vcat [ n; mark; (if hundred then mark else View.none) ]
          | _ -> n)
      in
      let column =
        View.vcat
        @@ List.init
             (Int.max 0 (height - 1))
             ~f:(fun i ->
               let i = i + 2 in
               let hundred = i % 100 = 0 in
               let i = i % 10 in
               let n = render ~hundred i in
               let mark = View.text ~attrs:(mark_attrs ~hundred) "──" in
               match i with
               | 0 -> View.hcat [ n; mark; (if hundred then mark else View.none) ]
               | _ -> n)
      in
      View.zcat [ row; View.pad ~t:1 column ]
    in
    View.zcat [ ruler_marks; View.pad ~l:2 ~t:2 view ]
  in
  let view =
    let%arr view
    and { width; height } = dimensions
    and flavor in
    let backdrop =
      View.rectangle
        ~width
        ~height
        ~attrs:[ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust) ]
        ()
    in
    View.zcat [ view; backdrop ]
  in
  ~view, ~handler:add_event
;;

module Event_attribution = struct
  type t =
    | Intercepted_by_diagnose
    | Handled_by_app of Source_code_position.t Nonempty_list.t option
end

module Event_entry = struct
  type t =
    { event : Event.t
    ; attribution : Event_attribution.t
    }
end

type t =
  { events : Event_entry.t list Bonsai.t
  ; record_event : (Event_entry.t -> unit Effect.t) Bonsai.t
  }

let create (graph @ local) =
  let events, inject_event =
    Bonsai.state_machine
      ~default_model:[]
      ~apply_action:(fun _ events event -> event :: List.take events 99)
      graph
  in
  let record_event =
    let%arr inject_event in
    fun event -> inject_event event
  in
  { events; record_event }
;;

let record_event t = t.record_event

let render_attribution ~flavor = function
  | Event_attribution.Intercepted_by_diagnose ->
    text ~flavor ~fg:Overlay2 " (intercepted by Diagnose)"
  | Handled_by_app None -> text ~flavor ~fg:Overlay2 " (ignored by app)"
  | Handled_by_app (Some positions) ->
    let { Source_code_position.pos_fname; pos_lnum; _ } = Nonempty_list.hd positions in
    text
      ~flavor
      ~fg:Overlay2
      [%string " @ %{Filename.basename pos_fname}:%{pos_lnum#Int}"]
;;

let panel { events; record_event; _ } ~app =
  let open Bonsai_term_tile_layout in
  tile_with_context
    ~key:"diagnose-panel"
    (fun context ~dimensions ~is_focused (graph @ local) ->
       let flavor = Bonsai_term_color_scheme.flavor graph in
       let output =
         let%arr { Dimensions.width = panel_width; height = panel_height } = dimensions
         and context
         and events
         and is_focused
         and flavor
         and record_event in
         let app_size =
           match context.Context.dimensions_of app with
           | None -> "hidden"
           | Some { Dimensions.width; height } -> [%string "%{width#Int}x%{height#Int}"]
         in
         let title =
           text
             ~flavor
             ~attrs:(if is_focused then [ Attr.bold ] else [])
             (if is_focused then " ▶ Diagnose " else "   Diagnose ")
         in
         let event_section = section ~flavor "Event log: " in
         let event_views =
           match events with
           | [] -> [ text ~flavor ~fg:Overlay2 "  (events will appear here)" ]
           | events ->
             List.map events ~f:(fun { Event_entry.event; attribution } ->
               View.pad
                 ~l:2
                 (View.hcat
                    [ render_event ~flavor event; render_attribution ~flavor attribution ]))
         in
         let body =
           View.vcat
             ([ View.hcat
                  [ section ~flavor "App size: "
                  ; text ~flavor ~fg:Mauve ~attrs:[ Attr.bold ] app_size
                  ]
              ; View.hcat [ section ~flavor "Text styles: "; style_samples ~flavor ]
              ; View.hcat [ event_section ]
              ; View.hcat
                  [ section ~flavor "Recording events in: "
                  ; text
                      ~flavor
                      ~attrs:[ Attr.bold ]
                      ~fg:Mauve
                      (if is_focused then "Diagnose" else "App")
                  ]
              ]
              @ event_views)
         in
         let view = View.vcat [ title; body ] in
         let backdrop =
           View.rectangle
             ~attrs:[ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust) ]
             ~width:panel_width
             ~height:panel_height
             ()
         in
         let handler event =
           match event with
           | event ->
             Captured_or_ignored.capture
               (record_event { Event_entry.event; attribution = Intercepted_by_diagnose })
         in
         ~view:(View.zcat [ view; backdrop ]), ~handler
       in
       output)
;;
