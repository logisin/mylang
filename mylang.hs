{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Exts (Constraint)
import Control.Monad(ap)

data Expr a where
  I   :: Int -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int->Expr Int
  Mul :: Expr Int -> Expr Int->Expr Int
  Eq  :: Eval a => Expr a -> Expr a -> Expr Bool



{-data NF :: (* -> Constraint) -> (* -> *) -> * -> * where 
  FMap :: c x => (x->a)-> t x -> NF c t a

instance Functor (NF c t) where
  fmap :: (a->b)->NF c t a -> NF c t b
  fmap g (FMap h tx) = FMap ( g . h) tx

liftNF :: c a => t a -> NF c t a
liftNF ta = FMap id ta

lowerNF :: (forall x . c x => (x->a)-> t x-> t a) -> NF c t a-> t a
lowerNF fmp (FMap g tx) = fmp g tx
-}
data ExprNF :: (* -> Constraint) -> (* -> *) -> * -> * where
  FMap :: Eq x => (x->a) -> Expr x -> ExprNF Eq Expr a

instance Functor (ExprNF Eq Expr) where
   fmap :: (a->b)->ExprNF Eq Expr a -> ExprNF Eq Expr b
   fmap g (FMap h tx) = FMap (g . h) tx

liftNF :: Eq a => Expr a -> ExprNF Eq Expr a
liftNF ta = FMap id ta

lowerNF :: (forall x . Eq x => (x->a)-> Expr x -> Expr a) -> ExprNF Eq Expr a -> Expr a
lowerNF fmp (FMap g tx) = fmp g tx

f :: Eval x=>(x->Bool) -> Expr x-> Expr Bool
f g e = B( g(eval e))

data ExprM a where
 Return :: a -> ExprM a
 Bind   :: Eval x=>Expr x -> (x-> ExprM a) -> ExprM a

instance Functor ExprM where
  fmap f xs = xs >>= return . f

instance Applicative ExprM where
  pure = return
  (<*>) = ap

instance Monad ExprM where
 return :: a-> ExprM a
 return = Return

 (>>=) :: ExprM a -> (a-> ExprM b) -> ExprM b
 (Return a) >>= k = k a
 (Bind sx h) >>= k = Bind sx (\x-> h x>>=k) 

liftExpr :: Eval a=> Expr a -> ExprM a
liftExpr sa = Bind sa Return

bindSet:: Eval a=>Expr a-> (a->Expr b) -> Expr b
bindSet sa k = k(eval sa)

lowerExpr :: (Reval a) => ExprM a -> Expr a
lowerExpr (Return a) = reval a
lowerExpr (Bind sx k) = bindSet sx (lowerExpr . k)


{-eval :: Eq a => Expr a-> a
eval a = 
  case a of
   I i -> i
   B b -> b
   Add a b -> eval a + eval b
   Mul a b -> eval a * eval b
   Eq a b -> (eval a) == (eval b)
-}

class Eq a=> Eval a where
  eval :: Expr a-> a

instance Eval Int where
  eval (I i) = i
  eval (Add e1 e2) = eval e1 + eval e2
  eval (Mul e1 e2) = eval e1 * eval e2

instance Eval Bool where
  eval (B b) = b
  eval (Eq e1 e2) = eval e1 == eval e2


class Eq a=>Reval a where
   reval :: a-> Expr a

instance Reval Int where
   reval i =  I i

instance Reval Bool where
   reval b = B b


s1 :: ExprM Int
s1 = do let a=I 1
        let b=I 2
        liftExpr (Add a b)
        

