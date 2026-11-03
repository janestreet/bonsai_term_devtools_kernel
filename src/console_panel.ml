open! Core
open Bonsai_term
open Bonsai.Let_syntax
module Scroller = Bonsai_term_scroller
module Tile_layout = Bonsai_term_tile_layout

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

let render_message ~flavor (message : Expert.Console_debug.Message.t) =
  let fg : Bonsai_term_color_scheme.t =
    match message.level with
    | Some `Error -> Red
    | Some `Warn -> Yellow
    | Some (`Debug | `Info) | None -> Text
  in
  let timestamp = Expert.Console_debug.Message.ofday_string message in
  match String.split_lines message.message with
  | [] -> text ~flavor ~fg:Overlay1 timestamp
  | first :: rest ->
    let indent = String.make (String.length timestamp + 1) ' ' in
    View.vcat
      (View.hcat
         [ text ~flavor ~fg:Overlay1 [%string "%{timestamp} "]; text ~flavor ~fg first ]
       :: List.map rest ~f:(fun line -> text ~flavor ~fg (indent ^ line)))
;;

let render_messages ~flavor = function
  | [] -> text ~flavor ~attrs:[ Attr.italic ] " No console messages yet. "
  | messages -> View.vcat (List.map messages ~f:(render_message ~flavor))
;;

let panel () =
  Tile_layout.tile ~key:"console-panel" (fun ~dimensions ~is_focused (local_ graph) ->
    (* Flush buffered messages from the last frame. *)
    Bonsai.Edge.after_display
      (Bonsai.return (Effect.of_sync_fun Expert.Console_debug.flush_now ()))
      graph;
    let flavor = Bonsai_term_color_scheme.flavor graph in
    let scroller_dimensions =
      let%arr { Dimensions.width; height } = dimensions in
      { Dimensions.width; height = Int.max 0 (height - 1) }
    in
    let content =
      let%arr messages = Expert.Console_debug.messages
      and flavor in
      render_messages ~flavor messages
    in
    let scroller =
      Scroller.component
        ~default_stuck_to_bottom:true
        ~crop_width_if_too_big:`Yes
        ~dimensions:scroller_dimensions
        content
        graph
    in
    (* Re-enable follow mode when the user scrolls back to the bottom. *)
    let stick_to_bottom =
      let%arr { Scroller.inject; scroll_position; stuck_to_bottom; _ } = scroller in
      match scroll_position, stuck_to_bottom with
      | Bottom, false -> inject Stick_to_bottom
      | _ -> Effect.Ignore
    in
    Bonsai.Edge.after_display stick_to_bottom graph;
    let%arr { Scroller.view = scroller_view; less_keybindings_handler; _ } = scroller
    and { width; height } = dimensions
    and is_focused
    and flavor in
    let title =
      text
        ~flavor
        ~attrs:(if is_focused then [ Attr.bold ] else [])
        (if is_focused then " ▶ Console " else "   Console ")
    in
    let backdrop =
      View.rectangle
        ~width
        ~height
        ~attrs:[ Attr.bg (Bonsai_term_color_scheme.color ~flavor Crust) ]
        ()
    in
    let handler event =
      if is_focused then less_keybindings_handler event else Captured_or_ignored.ignore
    in
    ~view:(View.zcat [ View.vcat [ title; scroller_view ]; backdrop ]), ~handler)
;;
