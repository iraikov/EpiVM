
{- Runge-Kutta methods for numerical integration of ordinary differential equations -}

include "Prelude.e"
include "option.e"
include "list.e"
include "rat.e"

{-
   Store a list of rational numbers as a common denominator, then a list
   of numerators, all stored as doubles: this lets us write the values in
   the source as exact rational numbers, yet we can take advantage of not
   having to do too many conversions and also of not having to multiply
   by 0 or 1 in some cases.
-}

{- type RCL = real * real list -}

compose (f: Fun, g: Fun) -> Any = \x: Any . f(g(x))

bigIntToFloat (x: BigInt) -> Float = strToFloat (bigIntToStr (x))

{- ratToRCL :: [rational] -> RCL -}
ratToRCL (xs: Data) -> Data =
  case xs of {
       Con 0 () -> Con 1 (1.0, Con 0 ())
     | Con 1 (h:Data, tl:Data) -> 
       let ds: Data    = map (denominator, xs) in
       let dp: BigInt  = foldl1 (mulBig, ds) in
       let ns: Data    = map (compose (numerator,\x : Data . mulRat (mkRat (dp), x)), xs) in
       let g:  BigInt  = foldl (gcd, dp, ns) in
       Con 1 (bigIntToFloat (divBig (dp, g)), 
              map (compose (bigIntToFloat, \x: BigInt . divBig (x,g)), ns))
     }

ratToRCLs (xs: Data) -> Data = map (ratToRCL, xs)

ratToReals (xs: Data) -> Data = map (fromRational, xs)

m_scale (sc_fn: Fun) -> Data =
  \(s: Float,v: Any) =
    if s == 0.0
    then (NONE)
    else (if s == 1.0
          then SOME(v)
	  else SOME(sc_fn (s,v)))


{- Helper function to sum a list of K_i, skipping
   un-necessary multiplications and additions -}

k_sum (sc_fn: Fun, sum_fn: Fun, h: Float) -> Data =
   \rcl : Data . \ks : Data .
    let ns = case rcl of { Con 1 (d,ns) => ns } in
    let ns_ks = zip (ns,ks) in
	sc_fn (divF (h,d), foldl1 sum_fn (mapPartial (m_scale sc_fn) ns_ks))


{-
fun k_sum (sc_fn: real * 'a -> 'a, 
	   sum_fn: 'a * 'a -> 'a, 
	   h: real) 
	  ((d,ns), ks) =
    let 
	val ns_ks = ListPair.zip (ns,ks)
    in
	sc_fn (Real./ (h,d), foldl1 sum_fn (List.mapPartial (m_scale sc_fn) ns_ks  ))
    end
-}
