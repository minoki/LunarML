signature TOKEN_STREAM =
sig
  type token
  type pos
  type stream
  val next: stream -> (token * pos * stream) option
  val initialPos: string -> pos
end
