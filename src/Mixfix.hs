module Mixfix
  ( PrecedenceGraph, Precedence (..), Associativity (..), Fixity (..)
  , Operator (..), SomeOperator (..), NamePart (..)
  , Expr (..), In (..), Out (..)
  , expr
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Type.Equality (testEquality)
import Data.Type.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import Type.Reflection (Typeable, (:~:)(..), typeOf)

import Parser (NamePart (..), Parser (..))

data Associativity
  = LeftAssociative
  | NonAssociative
  | RightAssociative

data Fixity
  = Prefix
  | Infix Associativity
  | Postfix
  | Closed

newtype Operator (arity :: Nat) = Operator
  { nameParts :: Typeable arity => Vec ('S arity) NamePart
  }

deriving stock instance Typeable arity => Eq (Operator arity)
deriving stock instance Typeable arity => Show (Operator arity)

data SomeOperator where
  SomeOperator :: Typeable arity => Operator arity -> SomeOperator

data Precedence where
  Precedence :: (Fixity -> [SomeOperator]) -> [Precedence] -> Precedence

type PrecedenceGraph = [Precedence]

data In where
  In :: Typeable arity => Operator arity -> Vec arity Expr -> In

instance Eq In where
  In op1 exps1 == In op2 exps2 =
    case typeOf op1 `testEquality` typeOf op2 of
      Nothing -> False
      Just Refl -> op1 == op2 && exps1 == exps2

deriving stock instance Show In

data Out
  = Similar Expr
  | Tighter Expr
  deriving stock (Eq, Show)

data Expr
  = ClosedEx In  -- ^ ((_))
  | PostEx Out In  -- ^ _(_))
  | PreEx In Out  -- ^ ((_)_
  | InNonEx Expr In Expr  -- ^ _(_)_
  | InLeftEx Out In Expr  -- ^ _(_)l_
  | InRightEx Expr In Out  -- ^ _(_)r_
  deriving stock (Eq, Show)

expr :: PrecedenceGraph -> Parser Expr
expr g = precs g g

precs :: PrecedenceGraph -> [Precedence] -> Parser Expr
precs _ []       = Fail
precs g (p : ps) = prec g p `Or` precs g ps

inner :: PrecedenceGraph -> [SomeOperator] -> Parser In
inner _ []                      = Fail
inner g (SomeOperator op : ops) = Map (In op) (expr g `Between` nameParts op) `Or` inner g ops

prec :: PrecedenceGraph -> Precedence -> Parser Expr
prec g (Precedence ops sucs) = foldl1 Or
  [ ClosedEx `Map` op Closed
  , InNonEx  `Map` pUp `App` op (Infix NonAssociative) `App` pUp
  , appR     `Map` Plus preRight `App` pUp
  , appL     `Map` pUp `App` Plus postLeft
  , Fail
  ]
  where
    op :: Fixity -> Parser In
    op = inner g . ops

    pUp :: Parser Expr
    pUp = precs g sucs

    preRight, postLeft :: Parser (Out -> Expr)
    preRight =
      let
        pre = PreEx `Map` op Prefix
        rInf = InRightEx `Map` pUp `App` op (Infix RightAssociative)
      in
      pre `Or` rInf
    postLeft =
      let
        post = (\op' e1 -> PostEx e1 op') `Map` op Postfix
        lInf = (\op' e2 e1 -> InLeftEx e1 op' e2) `Map` op (Infix LeftAssociative) `App` pUp
      in
      post `Or` lInf

    appR :: NonEmpty (Out -> Expr) -> Expr -> Expr
    appR fs e = foldrNE (\f e' -> f (Similar e')) (\f -> f (Tighter e)) fs

    appL :: Expr -> NonEmpty (Out -> Expr) -> Expr
    appL e fs = foldlNE (\e' f -> f (Similar e')) (\f -> f (Tighter e)) fs

foldrNE :: (a -> b -> b) -> (a -> b) -> NonEmpty a -> b
foldrNE f z xs = foldr f (z (NonEmpty.last xs)) (NonEmpty.init xs)

foldlNE :: (b -> a -> b) -> (a -> b) -> NonEmpty a -> b
foldlNE f z (x :| xs) = foldl f (z x) xs
