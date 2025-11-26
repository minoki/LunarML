structure Cmd =
struct
  type 'msg t = 'msg list

  val none = []

  fun ofMsg msg = [msg]
end

signature MODEL =
sig
  type model
  type msg

  val init : unit -> model * msg Cmd.t
  val update : msg -> model -> model * msg Cmd.t 
  (* val subscriptions : model -> msg Sub.t *)
  val view : model -> unit
end

functor MakeRuntime (M : MODEL) =
struct
  val state : M.model ref = ref (#1 (M.init()))

  fun runCmds (cmds : M.msg Cmd.t) =
    List.app dispatch cmds

  and dispatch (msg : M.msg) =
    let
      val (newModel, cmds) = M.update msg (!state)
    in
      state := newModel;
      runCmds cmds
    end

  fun init () =
    let
      val (initialModel, initialCmds) = M.init ()
      do state := initialModel
      do runCmds initialCmds
    in
      ()
    end

  fun runView () =
    M.view (!state)
end
