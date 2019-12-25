newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
  fmap f (MyTriple (x, y, z)) = MyTriple (f x, f y, f z)

instance Applicative MyTriple where
  pure a = MyTriple (a, a, a)
  (MyTriple (f, g, h)) <*> MyTriple (x, y, z) = MyTriple (f x, g y, h z)


data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

instance Functor Tree2 where
  fmap _ EmptyT2      = EmptyT2
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node xs a ys) = Node (fmap f xs) (f a) (fmap f ys)


instance Applicative Tree2 where
  pure a = Leaf a
  EmptyT2 <*> _ = EmptyT2
  (<*>) _ EmptyT2 = EmptyT2
  (Leaf f) <*> (Leaf a) = Leaf $ f a
  (Leaf f) <*> (Node _ a _) = Leaf $ f a
  (Node _ f _) <*> (Leaf a) = Leaf $ f a
  (Node lf f rf) <*> (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)