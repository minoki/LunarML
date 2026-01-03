structure Love =
struct
  open Lua

  (* Root love table *)
  val love = global "love"

  (* -------- Helpers for cleaner code -------- *)

  (* Run an SML callback and return empty Lua result *)
  fun noResult th = (th (); Vector.fromList [])

  (* Extract the nth argument from a Lua args vector *)
  fun getArg args n = Vector.sub (args, n)

  (* -------- Callback Registration -------- *)

  fun setLoad onLoad =
    setField (love, "load",
      Lua.function (fn _ =>
        noResult onLoad))

  fun setUpdate onUpdate =
    setField (love, "update",
      Lua.function (fn args =>
        let
          val dt = checkReal (getArg args 0)
        in
          noResult (fn () => onUpdate dt)
        end))

  fun setDraw onDraw =
    setField (love, "draw",
      Lua.function (fn _ =>
        noResult onDraw))

  fun setKeyPressed onKey =
    setField (love, "keypressed",
      Lua.function (fn args =>
        let
          val key = checkString (getArg args 0)
        in
          noResult (fn () => onKey key)
        end))

  fun setMousePressed onMouse =
    setField (love, "mousepressed",
      Lua.function (fn args =>
        let
          val mx = checkReal (getArg args 0)
          val my = checkReal (getArg args 1)
          val button = checkString (getArg args 2)
        in
          noResult (fn () => onMouse (mx, my, button))
        end))

  (* -------- love.graphics -------- *)

  structure Graphics =
  struct
    val graphics = field (love, "graphics")

    fun print (text, x, y) =
      call0 (field (graphics, "print"))
        (Vector.fromList [
          fromString text,
          fromReal x,
          fromReal y
        ])

    fun clear () =
      call0 (field (graphics, "clear"))
        (Vector.fromList [])

    datatype DrawMode = Fill | Line
    fun getMode Fill = "fill"
      | getMode Line = "line"

    fun rectangle (mode, x, y, w, h) =
      call0 (field (graphics, "rectangle"))
        (Vector.fromList [
          fromString (getMode mode),
          fromReal x,
          fromReal y,
          fromReal w,
          fromReal h
        ])
  end

  (* ------------- APP ------------- *)
  structure Config =
  struct
    type t = {
      load: (unit -> unit) option,
      update: (real -> unit) option,
      draw: (unit -> unit) option,
      keyPressed: (string -> unit) option,
      mousePressed: (real * real * string -> unit) option
    }

    val empty : t = {
      load = NONE,
      update = NONE,
      draw = NONE,
      keyPressed = NONE,
      mousePressed = NONE
    }

    fun withLoad f {load = _, update, draw, keyPressed, mousePressed} =
      { load = SOME f, update, draw, keyPressed, mousePressed }
    fun withUpdate f {load, update = _, draw, keyPressed, mousePressed} =
      { load, update = SOME f, draw, keyPressed, mousePressed }
    fun withDraw f {load, update, draw = _, keyPressed, mousePressed} =
      { load, update, draw = SOME f, keyPressed, mousePressed }
    fun withKey f {load, update, draw, keyPressed = _, mousePressed} =
      { load, update, draw, keyPressed = SOME f, mousePressed }
    fun withMouse f {load, update, draw, keyPressed, mousePressed = _} =
      { load, update, draw, keyPressed, mousePressed = SOME f }

    fun apply {load, update, draw, keyPressed, mousePressed} = fn () =>
    (
      Option.app setLoad load;
      Option.app setUpdate update;
      Option.app setDraw draw;
      Option.app setKeyPressed keyPressed;
      Option.app setMousePressed mousePressed
    )
  end
end
