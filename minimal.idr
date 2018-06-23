module Minimal


data TypeList : Type


data Compound = SomeString String | SomeType Type

typeList : List Compound
typeList = [SomeType Int, SomeString "hello"]

otherTypeList : List Compound
otherTypeList = [SomeType String, SomeType (List Int)]


-- list of types and return type make a function
ConvertTypes : List Compound -> Type -> Type
ConvertTypes [] ret = ret
ConvertTypes ((SomeString x) :: ts) ret = ConvertTypes ts ret
ConvertTypes ((SomeType x) :: ts) ret = x -> ConvertTypes ts ret

combinedLists : (List (List Compound))
combinedLists = [Minimal.typeList, Minimal.otherTypeList]

TypeListToType : (List (List Compound)) -> Type
TypeListToType [] = ()
TypeListToType (x :: xs) = (ConvertTypes x (), TypeListToType xs)


tryListTypes : TypeListToType Minimal.combinedLists
tryListTypes = ?tryListTypes_rhs
