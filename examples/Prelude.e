%include "string.h"

-- IO

%inline putStr (x:String) -> Unit =
    foreign Unit "putStr" (x:String)

putStrLn (x:String) -> Unit =
    putStr(append(x,"\n"))

readStr () -> String =
    foreign String "readStr" ()

intToStr (x:Int) -> String =
    foreign String "intToStr" (x:Int)

strToInt (x:String) -> Int =
    foreign Int "strToInt" (x:String)

printInt (x:Int) -> Unit =
    foreign Unit "printInt" (x:Int)

strToFloat (x:String) -> Float =
    foreign Float "strToFloat" (x:String)

floatToStr (x:Float) -> String =
    foreign String "floatToStr" (x:Float)

-- String operations

append (x:String, y:String) -> String =
    foreign String "append" (x:String, y:String)

length (x:String) -> String =
    foreign Int "strlen" (x:String)

index (x:String, i:Int) -> Char =
    foreign Int "strIndex" (x:String, i:Int)

-- Big number arithmetic

addBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "addBig" (x:BigInt, y:BigInt)

subBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "subBig" (x:BigInt, y:BigInt)

mulBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "mulBig" (x:BigInt, y:BigInt)

modBig (x:BigInt,y:BigInt) -> BigInt =
    foreign BigInt "modBig" (x:BigInt,y:BigInt)

divBig (x:BigInt,y:BigInt) -> BigInt =
    foreign BigInt "divBig" (x:BigInt,y:BigInt)

eqBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "eqBig" (x:BigInt, y:BigInt)

ltBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "ltBig" (x:BigInt, y:BigInt)

gtBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "gtBig" (x:BigInt, y:BigInt)

leBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "leBig" (x:BigInt, y:BigInt)

geBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "geBig" (x:BigInt, y:BigInt)

printBig (x:BigInt) -> Unit =
   foreign Unit "printBig" (x:BigInt)

bigIntToStr (x:BigInt) -> String =
    foreign String "bigToStr" (x:BigInt)

strToBigInt (x:String) -> Int =
    foreign String "strToBig" (x:String)

