module Pretty
  ( prettyExpr, prettyEx, prettyIn, prettyOut, prettyNamePart
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter (Doc, pretty, (<+>))

import Mixfix
  ( Ex (..), Expr (..), In (..), NamePart (..), Operator (..), Out (..)
  )

prettyExpr :: Expr -> Doc ann
prettyExpr (Expr ex) = prettyEx ex

prettyEx :: Ex -> Doc ann
prettyEx = \case
  ClosedEx in' -> prettyIn in'
  PostEx out in' -> prettyOut out <+> prettyIn in'
  PreEx in' out -> prettyIn in' <+> prettyOut out
  InNonEx exprL in' exprR -> prettyExpr exprL <+> prettyIn in' <+> prettyExpr exprR
  InLeftEx out in' expr -> prettyOut out <+> prettyIn in' <+> prettyExpr expr
  InRightEx expr in' out -> prettyExpr expr <+> prettyIn in' <+> prettyOut out

prettyIn :: In -> Doc ann
prettyIn (In (Operator ns') exprs) = go (NonEmpty.toList ns') exprs
  where
    go :: [NamePart] -> [Expr] -> Doc ann
    go [n] [] = prettyNamePart n
    go (n : ns) (x : xs) = prettyNamePart n <+> prettyExpr x <+> go ns xs
    go _ _ = undefined

prettyOut :: Out -> Doc ann
prettyOut = \case
  Similar ex -> prettyEx ex
  Tighter expr -> prettyExpr expr

prettyNamePart :: NamePart -> Doc ann
prettyNamePart (NamePart n) = pretty n
