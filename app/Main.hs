module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Mixfix
  ( Associativity (..), Fixity (..), Operator (..), Precedence (..)
  , PrecedenceGraph, expr
  )
import Parser (NamePart (..), runParser)
import Pretty (prettyExpr)

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
