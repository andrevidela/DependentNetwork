module Network.Requests
import Data.List
%default total

data URLComp : Type where
  End : URLComp
  Arg : Type -> URLComp -> URLComp
  Empty: String -> URLComp -> URLComp

PathSig : URLComp -> Type
PathSig End = String
PathSig (Arg x xs) = x -> PathSig xs
PathSig (Empty x xs) = PathSig xs

data IsShowable : (t : Type) -> Type where
  ToShow : Show a => (a : t) -> IsShowable t

ListToSig : List Type -> Type
ListToSig [] = String
ListToSig (x :: xs) = x -> ListToSig xs

listToFunc : {t : Type} -> (l : List (t, t -> String)) -> String ->  ListToSig (map fst l)
listToFunc [] x = x
listToFunc ((a, f) :: xs) x = \arg => listToFunc xs ((f arg) ++ x)

components : URLComp
components = Arg String (Empty "user" (Arg Int (Empty "path" End)))

