module DefineAPI
import APIComponents

%default total

infixl 8 :<|>
infixl 8 :>:
infixl 8 :>

data HTTPMethod : Type where
  GET : HTTPMethod
  POST : (body: Type) -> HTTPMethod
  PUT : (body: Type) -> HTTPMethod
  DELETE : (body: Type) -> HTTPMethod
  PATCH : (body: Type) -> HTTPMethod

data HasBody : HTTPMethod -> Type where
  PostBody : (b : Type) -> HasBody (POST b)
  PutBody : (b : Type) -> HasBody (PUT b)
  DeleteBody : (b : Type) -> HasBody (DELETE b)
  PatchBody : (b : Type) -> HasBody (PATCH b)

getBody : HasBody m -> Type
getBody (PostBody b) = b
getBody (PutBody b) = b
getBody (DeleteBody b) = b
getBody (PatchBody b) = b

Uninhabited (HasBody GET) where
  uninhabited (PostBody _) impossible
  uninhabited (PutBody _) impossible
  uninhabited (DeleteBody _) impossible
  uninhabited (PatchBody _) impossible

data BodyView : HTTPMethod -> Type where
  NoBody : BodyView GET
  SomeBody : (m : HTTPMethod) -> {auto prf : HasBody m} -> BodyView m

methodBodyView : (m : HTTPMethod) -> BodyView m
methodBodyView GET = NoBody
methodBodyView (POST body) = SomeBody (POST body)
methodBodyView (PUT body) = SomeBody (PUT body)
methodBodyView (DELETE body) = SomeBody (DELETE body)
methodBodyView (PATCH body) = SomeBody (PATCH body)


record EndpointType where
  constructor MkEndpointType
  method : HTTPMethod
  path : URLPath
  returnType : Type


syntax [m] ":>" [p] ":>:" [r] = MkEndpointType m p r

ListToType : List Type -> Type -> Type
ListToType [] ret = ret
ListToType (t :: ts) ret = t -> ListToType ts ret

pathToList : URLPath -> List Type
pathToList [] = []
pathToList ((Empty x) :: xs) = pathToList xs
pathToList ((Printable t _) :: xs) = t :: pathToList xs 

GetPathType : EndpointType -> Type
GetPathType (method :> path :>: returnType) with (methodBodyView method)
  GetPathType (GET :> path :>: returnType) | NoBody = ListToType (pathToList path) returnType
  GetPathType (method :> path :>: returnType) | (SomeBody method) {prf} = 
    let body = getBody prf in
        ListToType (body :: (pathToList path)) returnType

ApiDef : Type
ApiDef = List EndpointType

record UserInfo where
  constructor MkUserInfo
  userID : Int
  username : String

myAPI : ApiDef
myAPI = [ GET :> ["v1", "payment"] :>: String
        , GET :> ["v1", "payment", ~Int] :>: String
        , (POST UserInfo) :>  ["v1", "payment", ~Int] :>: ()
        ]


ServerType : ApiDef -> Type
ServerType [] = ()
ServerType (x :: []) = GetPathType x
ServerType (x :: xs) = (GetPathType x, ServerType xs)


snd_fn : Int -> String
snd_fn = ?whut_1

myServer : GetPathType ((POST UserInfo) :>  ["v1", "payment", ~Int] :>: ())
myServer = ?idkanymore

returnString : IO String
returnString = pure "hello"

createPayment : UserInfo -> Int -> IO ()
createPayment user thing = putStrLn "creating payment"

