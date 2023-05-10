io.output("capture.sml")
io.write[[
fun t x = if x < 0 then t (x + 1) - 1 else x
fun f z
    = let val a0 = t z
]]
for i = 1, 300 do
  local seq = {}
  for j = 0, i - 1 do
    table.insert(seq, string.format("a%d", j))
  end
  io.write(string.format("          val a%d = t (List.last [%s])\n", i, table.concat(seq, ", ")))
end
io.write[[
          fun g (i, acc) = if i < 0 then
                               List.rev acc
                           else
                               g (i - 1, (fn () => i) :: acc)
      in g (a300, [])
      end;
List.app (fn u => print (Int.toString (u ()) ^ "\n")) (f 10);
]]
