import System.IO
import Ast
import FaultLocal

{-
Example program:

sum :: k:Int -> {v:Int | v>=0 && v>= k}
let rec sum k =
  if k <= 0 then 0
  else let s = sum (k-1) in s-k

this should be:
let rec sum k =
  if k <= 0 then 0
  else let s = sum (k-1) in s+k
-}

-- constraints
c1 = "k:k1, k<=0 |- {v=0} <: k_r"
c2 = "k:k1, ~(k<=0) |- k_e <: k_r"
c3 = "k:k1, ~(k<=0), s:[k-1|k]k_r |- {v=s+k} <: k_e"
c4 = "k:k1, ~(k<=0) |- {v=k-1} <: k_1"

-- program locations
p1 = LessEqual (Var "k") (IntLiteral 0)
p2 = IntLiteral 0
p3 = App (Var "sum") (Subtract (Var "k") (IntLiteral 1))
p4 = Add (Var "s") (Var "k")
p5 = Let "s" p3 p4

ctl = [
  (c1,[p1,p2]),
  (c2,[p1,p5]),
  (c3,[p1,p3,p4]),
  (c4,[p1,p3])]

-- need Liquid Types to automatically compute this
minset = [c3]

main :: IO ()
main = do
  let loc = show $ getFaultLocal ctl minset
  putStrLn $ "Possible fault location: " ++ loc


