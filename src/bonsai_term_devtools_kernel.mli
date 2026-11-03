open! Core
open Bonsai_term

(** In-terminal developer tools for Bonsai_term applications. *)

module Key_binding : sig
  type t

  (** A key and its modifiers. Modifier order and Ctrl-letter case are ignored when
      matching events. [display] overrides the text shown in the devtools header. *)
  val create : ?mods:Event.Modifier.t list -> ?display:string -> Event.Key.t -> t
end

module Key_bindings : sig
  type t

  (** The standard F12 bindings. *)
  val default : t

  (** No keyboard shortcuts. Devtools can still be controlled through [Controls]. *)
  val none : t

  (** Overrides individual groups of default bindings. Passing an empty list disables that
      group; an omitted argument retains its default. *)
  val create
    :  ?toggle:Key_binding.t list
    -> ?focus_forward:Key_binding.t list
    -> ?focus_backward:Key_binding.t list
    -> unit
    -> t
end

module Controls : sig
  (** A handle for accessing the controls of one devtools wrapper. Create the handle
      before constructing the app, pass it to [start], [start_with_exit], or
      [Expert.with_devtools], and use the accessors from within the wrapped app's Bonsai
      graph. Outside that scope, [is_open] is [false] and scheduling another control
      raises. *)
  type t

  val create : unit -> t
  val is_open : t -> Bonsai.graph @ local -> bool Bonsai.t
  val open_ : t -> Bonsai.graph @ local -> unit Effect.t Bonsai.t
  val close : t -> Bonsai.graph @ local -> unit Effect.t Bonsai.t
  val toggle : t -> Bonsai.graph @ local -> unit Effect.t Bonsai.t
end

(** [start app] starts [app] with in-terminal devtools. By default, [F12] opens and closes
    the devtools. Like [Bonsai_term.start], it exits when the user presses Ctrl-C.

    The arguments have the same meaning as for [start_with_exit]. *)
val start
  :  ?max_console_messages:int
  -> ?start_open:bool
  -> ?prepare_to_restart:unit Effect.t
  -> ?devtools_controls:Controls.t
  -> ?devtools_key_bindings:Key_bindings.t
  -> ?capture_global_log:bool
  -> ?additional_panels:Bonsai_term_tile_layout.Layout.t list
  -> ?dispose:bool
  -> ?nosig:bool
  -> ?mouse:Bonsai_term.Mouse_reporting.t
  -> ?bpaste:bool
  -> ?reader:Async.Reader.t
  -> ?writer:Async.Writer.t
  -> ?time_source:Async.Time_source.t
  -> ?optimize:bool
  -> ?target_frames_per_second:float
  -> ?for_mocking:Notty_async.For_mocking.t
  -> ?instrumentation:Bonsai_driver.Instrumentation.Packed.t
  -> (dimensions:Dimensions.t Bonsai.t
      -> Bonsai.graph @ local
      -> view:View.t Bonsai.t
         * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t)
  -> unit Async.Deferred.Or_error.t

(** [start_with_exit app] starts [app] with in-terminal devtools. [F12] opens and closes
    the devtools (unless overridden with custom key bindings).

    [max_console_messages] bounds how many messages the Console retains. Defaults to 200.

    [capture_global_log] routes [Log.Global] messages to the Console instead of its
    existing outputs, which prevents the default stderr output from corrupting the
    terminal display. Defaults to [true].

    [additional_panels] is intended for extensions such as the internally hosted web
    devtools. Each panel becomes a subtree of the devtools column.

    [start_open] opens the devtools without waiting for [F12]. Defaults to [false].

    [prepare_to_restart] lets you optionally specify an effect that runs before app exit
    as part of the devtools "restart" flow. This is useful for releasing resources that
    the new process will recreate (e.g., a child process or server listening on a port).

    [devtools_controls] is an optional caller-created handle for controlling devtools from
    within [app]. When omitted, the default keyboard controls still work normally.

    [devtools_key_bindings] replaces any of the default keyboard controls.

    The remaining optional arguments are forwarded to [Bonsai_term.start_with_exit]. *)
val start_with_exit
  :  ?max_console_messages:int
  -> ?start_open:bool
  -> ?prepare_to_restart:unit Effect.t
  -> ?devtools_controls:Controls.t
  -> ?devtools_key_bindings:Key_bindings.t
  -> ?capture_global_log:bool
  -> ?additional_panels:Bonsai_term_tile_layout.Layout.t list
  -> ?dispose:bool
  -> ?nosig:bool
  -> ?mouse:Bonsai_term.Mouse_reporting.t
  -> ?bpaste:bool
  -> ?reader:Async.Reader.t
  -> ?writer:Async.Writer.t
  -> ?time_source:Async.Time_source.t
  -> ?optimize:bool
  -> ?target_frames_per_second:float
  -> ?for_mocking:Notty_async.For_mocking.t
  -> ?instrumentation:Bonsai_driver.Instrumentation.Packed.t
  -> (exit:('exit -> unit Effect.t)
      -> dimensions:Dimensions.t Bonsai.t
      -> Bonsai.graph @ local
      -> view:View.t Bonsai.t
         * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t)
  -> 'exit Async.Deferred.Or_error.t

module Expert : sig
  (** Wraps an app with in-terminal devtools.

      [request_restart], when supplied, adds a Restart panel that schedules the given
      effect. The other devtools arguments have the same meaning as [start_with_exit]. *)
  val with_devtools
    :  ?max_console_messages:int
    -> ?start_open:bool
    -> ?request_restart:unit Effect.t
    -> ?devtools_controls:Controls.t
    -> ?devtools_key_bindings:Key_bindings.t
    -> ?capture_global_log:bool
    -> ?additional_panels:Bonsai_term_tile_layout.Layout.t list
    -> dimensions:Dimensions.t Bonsai.t
    -> (dimensions:Dimensions.t Bonsai.t
        -> Bonsai.graph @ local
        -> view:View.t Bonsai.t
           * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t)
    -> Bonsai.graph @ local
    -> view:View.t Bonsai.t * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
end

module For_testing : sig
  val with_devtools
    :  ?max_console_messages:int
    -> ?start_open:bool
    -> ?request_restart:unit Effect.t
    -> ?devtools_controls:Controls.t
    -> ?devtools_key_bindings:Key_bindings.t
    -> ?capture_global_log:bool
    -> ?additional_panels:Bonsai_term_tile_layout.Layout.t list
    -> dimensions:Dimensions.t Bonsai.t
    -> (dimensions:Dimensions.t Bonsai.t
        -> Bonsai.graph @ local
        -> view:View.t Bonsai.t
           * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t)
    -> Bonsai.graph @ local
    -> view:View.t Bonsai.t * handler:(Event.t -> Captured_or_ignored.t Effect.t) Bonsai.t
end
