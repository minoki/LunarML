signature ORD_MAP_X =
sig
  include ORD_MAP
  val unionWithSecond: 'a map * 'a map -> 'a map
end
