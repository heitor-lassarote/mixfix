{- AUTOCOLLECT.TEST -}
module Test.Mixfix.Roundtrip
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Mixfix
  ( Associativity (..), Fixity (..), Operator (..), Precedence (..)
  , PrecedenceGraph, expr
  )
import Parser (NamePart (..), runParser)
import Pretty (prettyExpr)

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))

mkTest :: TestName -> Text -> TestTree
mkTest testName input =
  testCase testName $
    case runParser input (expr graph) of
      Nothing ->
        assertFailure "Parser failed to run"
      Just ex ->
        case runParser (Text.pack $ show $ prettyExpr ex) (expr graph) of
          Nothing ->
            assertFailure "Parser failed to run on roundtrip"
          Just ex' ->
            ex @=? ex'

test :: TestTree
test =
  testGroup "Roundtrip: Parse -> Pretty print -> Parse -> Compare" $ map (uncurry mkTest)
    [ ("Terminal", "0")
    , ("Sum of terminals", "0 + 1")
    , ("Parentheses", "(0)")
    , ("If expression", "if 0 then 1 else 2")
    ]

terminals :: [Operator]
terminals = parentheses : digits
  where
    parentheses :: Operator
    parentheses = Operator $ "(" :| [")"]

    digits :: [Operator]
    digits = map (\x -> Operator (NamePart (Text.pack $ show x) :| [])) [0 .. 9 :: Int]

terminalsPrec :: Precedence
terminalsPrec = Precedence (\case Closed -> terminals; _ -> []) []

ifThenElse :: Operator
ifThenElse = Operator $ "if" :| ["then", "else"]

addSubtract :: [Operator]
addSubtract = [Operator ("+" :| []), Operator ("-" :| [])]

graph :: PrecedenceGraph
graph =
  [ Precedence
    (\case Infix LeftAssociative -> addSubtract; _ -> [])
    [terminalsPrec]
  , Precedence
    (\case Prefix -> [ifThenElse]; _ -> [])
    [terminalsPrec]
  , terminalsPrec
  ]
