module APIComponents

import Data.List
%default total

public export
data PathComp : Type where
  Empty : String -> PathComp
  Printable : (t : Type) -> (t -> String) -> PathComp

Str' : PathComp
Str' = Printable String id

public export
data Showable : Type -> Type where
  ToShow : Show a => Showable a

public export implicit
emptyPath : String -> PathComp
emptyPath = Empty

-- I wish I could make this definition implicit but Idris can't 
-- find where to apply it correctly
--implicit
public export
typeToPath : (t : Type) -> {auto prf : Showable t} -> PathComp
typeToPath t {prf = ToShow} = Printable t show

syntax "~" [e] = typeToPath e


public export
URLPath : Type
URLPath = List PathComp

public export
URLPathToSig : URLPath -> Type -> Type
URLPathToSig [] t = t
URLPathToSig ((Empty x) :: xs) t = URLPathToSig xs t
URLPathToSig ((Printable t f) :: xs) t'= t -> URLPathToSig xs t' 

pathToAccGeneric : (Applicative acc, Semigroup (acc String)) => (p : URLPath) -> (acc String) -> URLPathToSig p (acc String)
pathToAccGeneric [] x = x
pathToAccGeneric ((Empty e) :: xs) acc = pathToAccGeneric xs (acc <+> (pure e))
pathToAccGeneric ((Printable t f) :: xs) acc = \arg => pathToAccGeneric xs (acc <+> (pure $ f arg))

pathToAcc : (Applicative acc, Monoid (acc String)) => (p : URLPath) -> URLPathToSig p (acc String)
pathToAcc p = pathToAccGeneric p neutral

PathToStringAcc : (p : URLPath) -> String -> URLPathToSig p String
PathToStringAcc [] x = x
PathToStringAcc ((Empty y) :: xs) acc = PathToStringAcc xs (acc ++ "/" ++ y)
PathToStringAcc ((Printable t f) :: xs) acc = \arg => PathToStringAcc xs (acc ++ "/" ++ (f arg))

export
pathToStringFunc : (p : URLPath) -> URLPathToSig p String
pathToStringFunc p = PathToStringAcc p ""

unitTest : String
unitTest = pathToStringFunc ["username", Str', "id", ~Int] "abc" 0

unitTest1 : List String
unitTest1 = pathToAccGeneric ["username", Str', "id", ~Int] [] "mark" 0
