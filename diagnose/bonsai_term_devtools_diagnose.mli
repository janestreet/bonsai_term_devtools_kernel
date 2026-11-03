open! Core
open Bonsai_term

(** Renders diagnostic information about the terminal environment. *)
val app
  :  dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

module Event_attribution : sig
  type t =
    | Intercepted_by_diagnose
    | Handled_by_app of Source_code_position.t Nonempty_list.t option
end

module Event_entry : sig
  type t =
    { event : Event.t
    ; attribution : Event_attribution.t
    }
end

type t

(** Create a state machine for the Diagnose panel's event log and recording state. *)
val create : Bonsai.graph @ local -> t

(** Records an event when recording is enabled, or does nothing otherwise. *)
val record_event : t -> (Event_entry.t -> unit Effect.t) Bonsai.t

(** A compact panel showing the wrapped application's dimensions, text styles, and recent
    input events. [app] should be the tile containing the wrapped application; the panel
    looks up its dimensions through the layout context. *)
val panel : t -> app:Bonsai_term_tile_layout.Key.t -> Bonsai_term_tile_layout.Layout.t
