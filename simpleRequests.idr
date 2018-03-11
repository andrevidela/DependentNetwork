import Data.List
%default total

data PathComp : Type where
  Empty : String -> PathComp
  Printable : (t : Type) -> (t -> String) -> PathComp

Str' : PathComp
Str' = Printable String id

data Showable : Type -> Type where
  ToShow : Show a => Showable a

implicit
emptyPath : String -> PathComp
emptyPath = Empty

--implicit
typeToPath : (t : Type) -> {auto prf : Showable t} -> PathComp
typeToPath t {prf = ToShow} = Printable t show

syntax "~" [e] = typeToPath e

URLPath : Type
URLPath = List PathComp

URLPathToSig : URLPath -> Type
URLPathToSig [] = String
URLPathToSig ((Empty x) :: xs) = URLPathToSig xs
URLPathToSig ((Printable t f) :: xs) = t -> URLPathToSig xs

pathToFuncAcc : (p : URLPath) -> String -> URLPathToSig p
pathToFuncAcc [] x = x
pathToFuncAcc ((Empty y) :: xs) acc = pathToFuncAcc xs (acc ++ "/" ++ y)
pathToFuncAcc ((Printable t f) :: xs) acc = \arg => pathToFuncAcc xs (acc ++ "/" ++ (f arg))

pathToFunc : (p : URLPath) -> URLPathToSig p
pathToFunc p = pathToFuncAcc p ""

unitTest : String
unitTest = pathToFunc ["username", Str', "id", ~Int] "abc" 0
