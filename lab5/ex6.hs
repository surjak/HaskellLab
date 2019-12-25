-- newtype Box a = MkBox a deriving Show
{-# LANGUAGE DeriveFunctor #-}
newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)


data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
  fmap _ EmptyBT        = EmptyBT
  fmap f (NodeBT a xs ys) = NodeBT (f a) (fmap f xs) (fmap f ys)


newtype Pair b a = Pair { getPair :: (a,b) } -- fmap should change the first element

instance Functor (Pair b) where
  fmap f (Pair (a, b)) = Pair (f a, b)


data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
  fmap _ EmptyT2      = EmptyT2
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node xs a ys) = Node (fmap f xs) (f a) (fmap f ys)


data GTree a = GLeaf a | GNode [GTree a] deriving Show

instance Functor GTree where
    fmap f (GLeaf a) = GLeaf (f a)
    fmap f (GNode xs) = GNode (fmap (fmap f) xs)

