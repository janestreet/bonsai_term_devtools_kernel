open! Core

(** A scrollable console with debug logs captured by [Bonsai_term.Debug]. *)
val panel : unit -> Bonsai_term_tile_layout.Layout.t
