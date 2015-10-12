{-# OPTIONS -Wall -Werror #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

-- | Monomorphic example demonstrating ideas from "Reflection without Remorse
-- paper" (<http://okmij.org/ftp/Haskell/zseq.pdf>). code below take hints from
-- <http://okmij.org/ftp/Haskell/Reflection.html#list-appends>.
module Research.Algorithms.Data.Zseq(
  AbsSeq, AbsSeqImpl, View
  , showRegular
  , showCPS
  , showZseq
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence ((><),ViewL(..), (<|), viewl, Seq)

import Prelude hiding ((++), showList, pure, foldr)

-- | the abstract interface for sequences
class AbsSeq base where
  -- | constructor 1: the empty sequence
  nil :: base a
  -- | constructor 2: add an element to a sequence
  cons :: a -> base a -> base a
  -- | deconstructor: either a sequence is empty, or we can extract a pair of
  -- (head, tail) from it
  deCons :: base a -> Maybe (a, base a)
  -- | fold: the ability to make a pass over the sequence
  foldr :: (a -> f b -> f b) -> f b -> base a -> f b

-- | common "template" for various implementations
class AbsSeqImpl impl (base :: * -> *) where
  -- | the "view" type for an implementation
  type View impl base :: * -> *

  -- | the ability to go from implementation type to the view type
  toView :: impl base a -> View impl base a
  -- | the ability to for from view type to the implementation type
  fromView :: View impl base a -> impl base a
  -- | the binary, associative, asymmetric operation in question. note that it
  -- operates on the implementation datatypes
  (++) :: impl base a -> impl base a -> impl base a

-- {{{ showSeq + helpers

-- | helper function to convert string to an abstract sequence. used for the
-- examples to pretty print sequences which showcase "reflection"
string2Seq :: AbsSeq base => String -> base Char
string2Seq x = case x of
  "" -> nil
  (h:t) -> cons h (string2Seq t)

-- | the example to pretty print sequences which showcases
-- "reflection". reflection here is defined as the need to inspect and modify
-- intermediate results during the midst of a computation.
--
-- the code below can be understood on a per case bases. if the sequence is
-- empty then the result of prety-printing it is quite simply "[]". otherwise,
-- calculate the result of pretty-printing the tail and then splice the result
-- of pretty-printing the head in between. when splicing, if the result of
-- pretty-printing the tail is "[]" don't append a comma, otherwise do.
--
-- rough complexity analysis:
-- T(n) = O('deCons') + T(n-1) + O('++')
showSeq :: (Eq (impl base Char), Show a, AbsSeqImpl impl base,
                AbsSeq (impl base)) =>
               List a -> impl base Char
showSeq xs = case (deCons xs) of
  Nothing -> cons '[' (cons ']' nil)
  Just(h, t) ->
    let Just(p, s) = deCons(showSeq t)
    in
     cons p (string2Seq(show h) ++ (if s == (cons ']' nil) then s
                                    else cons ',' nil ++ s))

-- }}}

-- {{{ List

-- | the traditional list datatype. note that since it implements AbsSeq, there
-- is no need to export the constructors. in fact, it's preferred not to.
data List a = Nil -- ^ the empty list
              | Cons a (List a) -- ^ append an element to a list
              deriving (Eq, Show)

-- | an implementation of abstract sequence interface for 'List'.
-- 'nil' = O(1),
-- 'cons' = O(1),
-- 'deCons' = O(1),
-- 'foldr' = O(n).
instance AbsSeq List where
  nil = Nil -- O(1)
  cons = Cons -- O(1)
  deCons xs = case xs of -- O(1)
    Nil -> Nothing
    Cons h t -> Just (h, t)
  foldr f ys xs = case xs of -- O(n)
    Nil -> ys
    Cons h t -> f h (foldr f ys t)

-- }}}

-- {{{ Id

-- | datatype to represent scenario when our view type is the same as our base
-- type.
newtype Id base a = Id { unId :: base a }
                       deriving (Eq, Show)

-- | details of the 'Id' strategy for implementing sequences. note that '++' is
-- defined in terms of 'foldr' which in the best case will be O(n), but in the
-- worst case (when the left argument is recursively constructed using '++') can
-- be O(n^2).
instance AbsSeq base => AbsSeqImpl Id base where
  type View Id base = base

  toView = unId -- O(1)
  fromView = Id -- O(1)
  -- O(n)
  (Id xs) ++ ys = foldr (\a (Id l) -> Id(cons a l)) ys xs

-- | an implementation of abstract sequence interface for 'Id' view type. as can
-- be seen all we do is convert to/from view representation.
-- 'nil' = O(1),
-- 'cons' = O(1),
-- 'deCons' = O(1),
-- 'foldr' = O(n).
instance AbsSeq base => AbsSeq (Id base) where
  nil = fromView nil
  cons a xs = fromView(cons a (toView xs))
  deCons xs = case (deCons (toView xs)) of
    Nothing -> Nothing
    Just(h, t) -> Just(h, Id t)
  foldr f z xs = foldr f z (toView xs)

-- | specializing the pretty-printing example.
--
-- T(n) = O('deCons') + T(n-1) + O('++')
--
-- T(n) = O(n) + O(n) + T(n-1)
--
-- T(n) = O(n^2)
showRegular :: Show a => List a -> Id List Char
showRegular = showSeq

-- }}}

-- {{{ CPS

-- | datatype to represent the CPS transformation
newtype CPS base a = CPS { unCPS :: base a -> base a }

-- | details of the 'CPS' transformation for sequences. note that '++' is
-- defined in terms of function composition and is thus O(1). note too that the
-- '++' is no longer asymmetric in its arguments.
instance AbsSeq base => AbsSeqImpl CPS base where
  type View CPS base = base

  toView dl = (unCPS dl) nil -- O(n)
  fromView l = CPS(\t -> unId((Id l) ++ (Id t))) -- O(1)
  (CPS xs) ++ (CPS ys) = CPS(xs . ys) -- O(1)

-- | an implementation of abstract sequence interface for 'CPS' transform of our
-- base type.
-- 'nil' = O(1),
-- 'cons' = O(1),
-- 'deCons' = O(n),
-- 'foldr' = O(n).
instance AbsSeq base => AbsSeq (CPS base) where
  nil = CPS id -- O(1)
  cons a (CPS xs) = CPS(\b -> cons a (xs b)) -- O(1)
  deCons xs = case (deCons (toView xs)) of -- O(n)
    Nothing -> Nothing
    Just(a, t) -> Just(a, fromView t)
  foldr f z xs = foldr f z (toView xs) -- O(n)

-- | the pretty-printing example with the CPS transformation. note that simply
-- specializing 'showSeq' doesn't work, because we have not defined equality on
-- (CPS List Char). we could define equality on 'CPS' by converting to and from
-- view, but that could be wasteful. below we define 'showCPS' directly.
--
-- T(n) = O('deCons') + T(n-1) + O('toView') + O('++')
--
-- T(n) = O(n) + T(n-1) + O(n) + O(1)
--
-- T(n) = O(n) + T(n-1)
--
-- T(n) = O(n^2)
showCPS :: Show a => List a -> CPS List Char
showCPS xs = go(fromView xs)
  where
    -- T(n) = T(n-1) + O(n-1)
    --      = O(n-1) + O(n-2) ...
    --      = O(n^2)
    go :: Show a => CPS List a -> CPS List Char
    go ys = case (deCons ys) of
      Nothing -> cons '[' (cons ']' nil)
      Just(h, t) ->
        let Just(p, s) = deCons(toView(go t))
        in
         cons p nil ++ string2Seq(show h) ++
         fromView(unId(if s == (cons ']' nil) then (Id s)
                       else (cons ',' nil) ++ (Id s)))

-- }}}

-- {{{ Zseq

-- | the base data type implemented using an efficient catenable sequence data
-- structure. for present purposes we're using 'Seq' which has O(log(min(m, n)))
-- runtime, but there's nothing preventing us from using an O(1)
-- implementation. as such in the rough back of the envelope complexity analysis
-- we'll assume that we're using an O(1) catenable sequence internally.
--
-- TODO: use type-aligned package
data ListV' a = ListV' (Seq (ListV a))
              deriving (Eq, Show)
-- | the view data type. note that the recursive references reference
-- 'ListV''. the idea is to not export the constructors for 'ListV' (or even
-- 'ListV'' for that matter), and instead expose 'ListV'' as an abstract
-- sequence datatype.
data ListV a = NilV
               | ConsV a (ListV' a)
               deriving (Eq, Show)

-- | an implementation of abstract sequence interface for 'ListV''.
-- 'nil' = O(1),
-- 'cons' = O(1),
-- 'deCons' = O(1) (because by not exposing 'NilV' and 'ListV'' we ensure by
-- construction that we don't have long chains of embedded 'NilV's),
-- 'foldr' = O(n).
instance AbsSeq ListV' where
  nil = ListV' Seq.empty -- O(1)
  cons a (ListV' xs) = ListV'((ConsV a nil) <| xs) -- O(1)
  -- O(#NilV) = O(1)
  deCons (ListV' xs) = case (viewl xs) of
    EmptyL -> Nothing
    lv :< lvs -> case lv of
      NilV -> deCons(ListV' lvs)
      ConsV a (ListV' lvs1) -> Just(a, ListV'(lvs1 >< lvs))
  -- O(n)
  foldr f z lv' = case (deCons lv') of
    Nothing -> z
    Just(a, lv1') -> f a (foldr f z lv1')

-- | datatype to represent "reflection without remorse" strategy
newtype Zseq base a = Zseq { unZseq :: base a }
                    deriving (Eq, Show)

-- | details of the 'Zseq' transformation for sequences. note that '++' is
-- defined in terms of concatenation of the underlying efficient sequence
-- datatype and is thus O(1). note too that the '++' is no longer asymmetric in
-- its arguments. both things in common with the 'CPS' transformation.
instance AbsSeqImpl Zseq ListV' where
  type View Zseq ListV' = ListV

  toView x = case (deCons (unZseq x)) of -- O(deCons)
    Nothing -> NilV
    Just(a, lv') -> ConsV a lv'
  fromView lv = Zseq $ case lv of -- O(1)
    NilV -> nil
    ConsV a lv' -> cons a lv'
  Zseq(ListV' xs) ++ Zseq(ListV' ys) = Zseq(ListV' (xs >< ys)) -- O(1)

-- | an implementation of abstract sequence interface for 'Zseq' strategy.
-- 'nil' = O(1),
-- 'cons' = O(1),
-- 'deCons' = O(1),
-- 'foldr' = O(n).
instance AbsSeq (Zseq ListV') where
  nil = Zseq nil
  cons a (Zseq lv') = Zseq(cons a lv')
  deCons (Zseq lv') = case (deCons lv') of
    Nothing -> Nothing
    Just(a, lv'1) -> Just(a, Zseq lv'1)
  foldr f z (Zseq lv') = foldr f z lv'

-- | specializing the pretty-printing example. this is where we see the real
-- strength of this technique.
--
-- T(n) = O('deCons') + T(n-1) + O('++')
--
-- T(n) = O(1) + T(n-1) + O(1)
--
-- T(n) = O(n)
showZseq :: Show a => List a -> Zseq ListV' Char
showZseq = showSeq

-- }}}
