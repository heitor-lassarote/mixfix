module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vec.Lazy (Vec (..))

import Mixfix
  ( Associativity (..), Fixity (..), Operator (..), Precedence (..)
  , PrecedenceGraph, SomeOperator (..), expr
  )
import Parser (NamePart (..), runParser)
import Pretty (prettyExpr)

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

printTest :: Text -> IO ()
printTest i =
  case runParser i $ expr graph of
    Nothing ->
      putStrLn "Failed to run parser."
    Just ex -> do
      putStrLn $ "Produced AST: " <> show ex
      putStrLn $ "Prettified: " <> show (prettyExpr ex)

main :: IO ()
main = do
  input <- Text.getLine
  printTest input
