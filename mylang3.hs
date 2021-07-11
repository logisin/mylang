{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude(Eq,Int,Bool,(==),(*),(+),fromInteger)
data Expr a where
  I   :: Int -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int->Expr Int
  Mul :: Expr Int -> Expr Int->Expr Int
  Eq  :: Reval a => Expr a -> Expr a -> Expr Bool

class Eq a=>Reval a where
  reval :: a -> Expr a

instance Reval Int where
  reval = I
instance Reval Bool where
  reval = B

eval:: Expr a-> a
eval a =
   case a of
    I i -> i
    B b -> b
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Eq a b -> (eval a) == (eval b)

class  Monad m   where
    return :: Reval a => a -> m a
    (>>=) :: m a -> (a-> m b) -> m b

instance Monad Expr where
    return = reval
    e>>=k = k ( eval e)

expr :: Expr Int
expr = do
      let x = I 1
      let y = I 2
      Add x y

expr2 :: Expr Int
expr2 = do
      x<- I 1
      y<- I 2
      return (x+y)

expr3 :: Expr Bool
expr3 = do
     let a = I 1
     let b = I 2
     Eq a b

     
