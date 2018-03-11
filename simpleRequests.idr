import Data.List
%default total

data PathComp : Type where
  Empty : String -> PathComp
  Printable : (t : Type) -> (t -> String) -> PathComp

Str' : PathComp
Str' = Printable String id

data Showable : Type -> Type where
  ToShow : Show a => Showable a

implicit emptyPath : String -> PathComp
emptyPath = Empty

typeToPath : (t : Type) -> {auto prf : Showable t} -> PathComp
typeToPath t {prf = ToShow} = Printable t show

URLPath : Type
URLPath = List PathComp

URLPathToSig : URLPath -> Type
URLPathToSig [] = String
URLPathToSig ((Empty x) :: xs) = URLPathToSig xs
URLPathToSig ((Printable t f) :: xs) = t -> URLPathToSig xs

pathToFuncAcc : (p : URLPath) -> String -> URLPathToSig p
pathToFuncAcc [] x = x
pathToFuncAcc ((Empty y) :: xs) str = pathToFuncAcc xs (str ++ "/" ++ y)
pathToFuncAcc ((Printable t f) :: xs) str = \arg => pathToFuncAcc xs (str ++ "/" ++ (f arg))

pathToFunc : (p : URLPath) -> URLPathToSig p
pathToFunc p = pathToFuncAcc p ""
