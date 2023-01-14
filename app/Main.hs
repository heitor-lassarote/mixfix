module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text

import Mixfix
import Parser

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

zero, add, paren, if' :: Text
zero = "0"
add = "0 + 1"
paren = "(0)"
if' = "if 0 then 1 else 2"

main :: IO ()
main = do
  let go i = print $ runParser i $ expr graph
  go zero
  go add
  go paren
  go if'
