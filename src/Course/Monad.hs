{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad(
  Monad(..)
, join
, (>>=)  
, (<=<)
) where

import Course.Applicative hiding ((<*>))
import Course.Core
import Course.Functor
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
(<*>) ::
  Monad f =>
  f (x -> y)
  -> f x
  -> f y
-- We want to use bind, =<<, which needs (a->f b) and f a, and produces f b.
-- <*> needs to produce f y. So let's make b = y.
-- and make a = x -> y.
-- Then if we need a->f b, we need a function which takes x->y as an argument
-- and returns f y. Well, creating a function is easy, (\xy -> ?).
-- And to return f y from x->y (and we also have an f x), is just applying x->y to f x
-- ie. xy <$> f x.
fxy <*> fx = (\xy -> xy <$> fx) =<< fxy
fxy <*> fx = fxy >>= (\xy -> xy <$> fx)  -- using flipped bind
-- or this very common pattern:
fxy <*> fx = 
  fxy >>= \xy ->
  fx  >>= \q ->
  pure (xy q)

infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Monad Id where
  (=<<) ::
    (a -> Id b)
    -> Id a
    -> Id b
  (=<<) =
    error "todo: Course.Monad (=<<)#instance Id"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) = flatMap

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) = bindOptional

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b))
    -> ((->) t a)
    -> ((->) t b)
-- (a -> t -> b) -> (t -> a) -> (t -> b)
  (atb =<< ta) t = atb (ta t) t  -- "if it compiles, it must be right"
  -- (atb =<< ta) t = ((atb) (ta t)) t  -- "if it compiles, it must be right"
  -- atb =<< ta = (\t -> (atb . ta) t)

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Monad f =>
  f (f a)
  -> f a
join x = id =<< x  -- or (=<< id)

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) = flip (=<<)

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
x <=< y = \a -> x =<< y a   -- haven't quite got this one yet...

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
