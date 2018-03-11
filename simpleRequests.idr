import Data.List
%default total

data PathComp = Empty String | Int' |  Str'

implicit emptyPath : String -> PathComp
emptyPath = Empty

URLPath : Type
URLPath = List PathComp


URLPathToSig : URLPath -> Type
URLPathToSig [] = String
URLPathToSig ((Empty x) :: xs) = URLPathToSig xs
URLPathToSig (Int' :: xs) = Int -> URLPathToSig xs
URLPathToSig (Str' :: xs) = String -> URLPathToSig xs

pathToFuncAcc : (p : URLPath) -> String -> URLPathToSig p
pathToFuncAcc [] x = x
pathToFuncAcc ((Empty y) :: xs) str = pathToFuncAcc xs (str ++ "/" ++ y)
pathToFuncAcc (Int' :: xs) str = \v => pathToFuncAcc xs (str ++ "/" ++ (show v))
pathToFuncAcc (Str' :: xs) str = \v => pathToFuncAcc xs (str ++ "/" ++ v)

pathToFunc : (p : URLPath) -> URLPathToSig p
pathToFunc p = pathToFuncAcc p ""
