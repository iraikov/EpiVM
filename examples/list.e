
map (f:Fun, xs:Data) -> Data
  = case xs of {
       Con 0 () -> Con 0 ()
     | Con 1 (h:Data, tl:Data) -> Con 1 (f(h), map(f, tl))
    }


foldl (f:Fun, xs:Data, init:Any) -> Any
  = case xs of {
       Con 0 () -> init
     | Con 1 (h:Data, tl:Data) -> foldl (f, tl, f(h,init))
    }


foldl1 (f:Fun, xs:Data) -> Any
  = case xs of {
       Con 0 () -> Con 0 ()
     | Con 1 (h:Data, tl:Data) -> case tl of {
            Con 0 () -> h
          | Con 1 (t:Data, rst:Data) -> foldl (f, rst, f(h,t))
       }
    }


zip (f:Any, xs:Data, ys:Data) -> Data
  = case xs of {
       Con 0 () -> Con 0 ()
     | Con 1 (x:Data, xs0:Data) -> case ys of {
           Con 0 () -> Con 0 ()
         | Con 1 (y:Data, ys0:Data) -> Con 1 (f(x,y), zip(f, xs0, ys0))
       }
    }

take (i:Int, x:Data) -> Data
  = if (i==0) then Con 0 () else
	case x of {
	   Con 0 () -> Con 0 ()
         | Con 1 (y:Any,ys:Data) -> Con 1 (y, take(i-1, ys))
        }


showList (f: Fun, x:Data) -> Data
  = case x of {
        Con 1 (y:Int, ys:Data) -> 
	   putStr(append(f(y),", "));
	   showList(ys)
      | Con 0 () -> putStrLn("nil")
    }
