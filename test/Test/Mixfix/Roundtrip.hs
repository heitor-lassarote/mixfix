{- AUTOCOLLECT.TEST -}
module Test.Mixfix.Roundtrip
  ( {- AUTOCOLLECT.TEST.export -}
  ) where

import Mixfix
  ( Associativity (..), Fixity (..), Operator (..), Precedence (..)
  , PrecedenceGraph, SomeOperator (..), expr
  )
import Parser (NamePart (..), runParser)
import Pretty (prettyExpr)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vec.Lazy (Vec (..))
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
    , ("Parentheses", "( 0 )")
    , ("If expression", "if 0 then 1 else 2")
    ]

terminals :: [SomeOperator]
terminals = parentheses : digits
  where
    parentheses :: SomeOperator
    parentheses = SomeOperator $ Operator $ "(" ::: ")" ::: VNil

    digits :: [SomeOperator]
    digits = map (\x -> SomeOperator $ Operator (NamePart (Text.pack $ show x) ::: VNil)) [0 .. 9 :: Int]

terminalsPrec :: Precedence
terminalsPrec = Precedence (\case Closed -> terminals; _ -> []) []

ifThenElse :: SomeOperator
ifThenElse = SomeOperator $ Operator $ "if" ::: "then" ::: "else" ::: VNil

addSubtract :: [SomeOperator]
addSubtract = [SomeOperator $ Operator ("+" ::: VNil), SomeOperator $ Operator ("-" ::: VNil)]

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
