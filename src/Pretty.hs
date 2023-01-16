module Pretty
  ( prettyExpr, prettyIn, prettyOut, prettyNamePart
  ) where

import Data.Type.Nat (Nat (..))
import Data.Vec.Lazy (Vec (..))
import Prettyprinter (Doc, pretty, (<+>))

import Mixfix
  ( Expr (..), In (..), NamePart (..), Operator (..), Out (..)
  )

prettyExpr :: Expr -> Doc ann
prettyExpr = \case
  ClosedEx in' -> prettyIn in'
  PostEx out in' -> prettyOut out <+> prettyIn in'
  PreEx in' out -> prettyIn in' <+> prettyOut out
  InNonEx exprL in' exprR -> prettyExpr exprL <+> prettyIn in' <+> prettyExpr exprR
  InLeftEx out in' expr -> prettyOut out <+> prettyIn in' <+> prettyExpr expr
  InRightEx expr in' out -> prettyExpr expr <+> prettyIn in' <+> prettyOut out

prettyIn :: In -> Doc ann
prettyIn (In (Operator ns') exprs) = go ns' exprs
  where
    go :: Vec ('S arity) NamePart -> Vec arity Expr -> Doc ann
    go (n ::: VNil) VNil = prettyNamePart n
    go (n ::: ns) (x ::: xs) = prettyNamePart n <+> prettyExpr x <+> go ns xs

prettyOut :: Out -> Doc ann
prettyOut = \case
  Similar expr -> prettyExpr expr
  Tighter expr -> prettyExpr expr

prettyNamePart :: NamePart -> Doc ann
prettyNamePart (NamePart n) = pretty n
