polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)


newtype CartesianCoord'' a = MkCartesianCoord'' (a,a) deriving Show
newtype PolarCoord'' a = MkPolarCoord'' (a,a) deriving Show

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) =
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

newtype Name'' = Name String deriving Show
newtype Surname'' = Surname String deriving Show
newtype Address'' = Address String deriving Show
type PersonInfo'' = (Name'', Surname'', Address'')
type PersonInfoToStringType'' = PersonInfo'' -> String


personInfoToString'' :: PersonInfoToStringType''
personInfoToString'' (Name nm, Surname snm, Address addr) =
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr