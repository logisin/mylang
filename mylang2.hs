{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Exts (Constraint)
import Control.Monad(ap)

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

reval1 :: a->Expr a
{-# SPECIALISE reval1 :: Int->Expr Int #-}
{-# SPECIALISE reval1 :: Bool->Expr Bool #-}
reval1 x = reval x
 
eval:: Expr a-> a
eval a =
   case a of
    I i -> i
    B b -> b
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Eq a b -> (eval a) == (eval b)
   
instance  Monad Expr where
   return x = reval1 x
   e1>>=k = k $ eval e1
