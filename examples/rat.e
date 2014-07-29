include "Prelude.e"

{- Poor man's rational arithmetic -}

main () -> Unit = 
           putStrLn (append (ratShow (mkFrac (3L, 4L)), 
                             (append (" = ", floatToStr (fromRational (mkFrac (3L, 4L)))))));
           putStrLn (append (ratShow (addRat (mkFrac (1L, 2L), mkFrac (3L, 4L))),
                             (append (" = ", floatToStr (fromRational (addRat (mkFrac (1L, 2L), mkFrac (3L, 4L))))))));
           putStrLn (append (ratShow (mulRat (mkFrac (1L, 2L), mkFrac (3L, 4L))),
                             (append (" = ", floatToStr (fromRational (mulRat (mkFrac (1L, 2L), mkFrac (3L, 4L))))))))


mkRat (x:BigInt) -> BigInt = Con 0 (x)

mkFrac (x:BigInt, y: BigInt) -> Data = Con 1 (x, Con 0 (y))


ratShow (r: Data) -> String = 
  case r of {
        
        Con 0 (x:BigInt) -> bigIntToStr (x)
      | Con 1 (x:BigInt, rst:Data) -> case rst of {
            Con 0 (y:BigInt) -> append (bigIntToStr (x), append ("//", bigIntToStr (y)))
        }
   }


fromRational (x: Data) -> Float =
  case x of {
        Con 0 (x:BigInt) -> strToFloat (bigIntToStr (x))
      | Con 1 (x:BigInt, rst:Data) -> case rst of {
            Con 0 (y:BigInt) -> ((strToFloat (bigIntToStr (x))) /.
                                 (strToFloat (bigIntToStr (y))))
        }
   }

    
numerator (x: Data) -> BigInt =
  case x of {
        Con 0 (x:BigInt) -> x
      | Con 1 (x:BigInt, rst:Data) -> x
   }
    

denominator (x: Data) -> BigInt =
  case x of {
        Con 0 (x:BigInt) -> 1L
      | Con 1 (x:BigInt, rst:Data) -> case rst of {
            Con 0 (y:BigInt) -> y
        }
   }


gcd (a: BigInt, b: BigInt) -> BigInt =
    if (eqBig(b,0L)) then a else gcd (b, modBig (a, b))


normalize (n:Data) -> Data =
  case n of {
        Con 0 (x:BigInt) -> n
      | Con 1 (x:BigInt, rst:Data) -> case rst of {
            Con 0 (y:BigInt) -> if (eqBig(y,0L)) 
                                then mkRat (0) else 
                                let c: BigInt = gcd (x, y)
                                 in 
                                    if (eqBig(c,y)) 
                                    then mkRat (divBig (x, c)) 
                                    else mkFrac (divBig (x, c), divBig (y, c))
        }
   }


addRat (x: Data, y: Data) -> Data = 

  let sym: Fun = \i : BigInt . \n : BigInt . \d : BigInt . mkFrac (addBig (n, mulBig (i, d)), d) in

  case x of {
        Con 0 (l:BigInt) -> 
        case y of {
                 Con 0 (r:BigInt) -> Con 0 (addBig (l, r))
               | Con 1 (n:BigInt, rst:Data) -> 
                   case rst of {
                              Con 0 (d:BigInt) -> sym (l, n, d)
                            }
               }
        | Con 1 (n:BigInt, rst:Data) -> 
        case rst of {
                   Con 0 (d:BigInt) -> 
                       case y of {
                                Con 0 (i:BigInt) -> sym (i, n, d)
                              | Con 1 (m:BigInt, rst:Data) -> 
                                  case rst of {
                                             Con 0 (e:BigInt) -> 
                                                 normalize (if eqBig (d, e) 
                                                            then mkFrac (addBig (n, m), d) 
                                                            else mkFrac (addBig (mulBig (n,e), mulBig (m,d)),
                                                                         mulBig (d,e)))
                                           }
                              }
                 }
         }


mulRat (x: Data, y: Data) -> Data = 

  let sym: Fun = \i : BigInt . \n : BigInt . \d : BigInt . mkFrac (mulBig (i, n), d) in

  case x of {
        Con 0 (l:BigInt) -> 
        case y of {
                 Con 0 (r:BigInt) -> Con 0 (mulBig (l, r))
               | Con 1 (n:BigInt, rst:Data) -> 
                   case rst of {
                              Con 0 (d:BigInt) -> sym (l, n, d)
                            }
               }
        | Con 1 (n:BigInt, rst:Data) -> 
        case rst of {
                   Con 0 (d:BigInt) -> 
                       case y of {
                                Con 0 (i:BigInt) -> sym (i, n, d)
                              | Con 1 (m:BigInt, rst:Data) -> 
                                  case rst of {
                                             Con 0 (e:BigInt) -> 
                                                 normalize (mkFrac (mulBig (n,m), mulBig (d,e)))
                                           }
                              }
                 }
         }
