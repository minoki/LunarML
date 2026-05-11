structure export = struct
  fun sieve (n : int) : int =
    let
      val composite = Array.array (n + 1, false)
      fun mark i step =
        if i > n then ()
        else ( Unsafe.Array.update (composite, i, true)
             ; mark (i + step) step
             )
      fun loop p count =
        if p > n then count
        else if Unsafe.Array.sub (composite, p) then
          loop (p + 1) count
        else
          ( mark (p * p) p
          ; loop (p + 1) (count + 1)
          )
    in
      loop 2 0
    end

  fun primesBelow100 () : int = sieve 99
  fun primesBelow1000 () : int = sieve 999
end
