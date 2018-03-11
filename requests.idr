import Data.List
%default total

-- data URLComponents : {t : Type} -> {a: t} -> Type where
--   Empty : String -> URLComponents -> URLComponents
--   Arg : Show a => t -> URLComponents -> URLComponents
--   End : URLComponents

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


-- PathToFunc : Show a => (p : URLComp a) -> PathSig p
-- PathToFunc End = ?PathToFunc_rhs_1
-- PathToFunc (Arg t x) = ?PathToFunc_rhs_2
-- PathToFunc (Empty x y) = ?PathToFunc_rhs_3



components : URLComp
components = Arg String (Empty "user" (Arg Int (Empty "path" End)))

