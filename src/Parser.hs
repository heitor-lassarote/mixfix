module Parser
  ( Parser (..), runParser
  , NamePart (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text

newtype NamePart = NamePart
  { namePart :: Text
  } deriving newtype (IsString)
    deriving stock (Eq, Show)

data Parser a where
  Fail :: Parser a
  Or :: Parser a -> Parser a -> Parser a
  App :: Parser (a -> b) -> Parser a -> Parser b
  Map :: (a -> b) -> Parser a -> Parser b
  Plus :: Parser a -> Parser (NonEmpty a)
  Between :: Parser a -> NonEmpty NamePart -> Parser [a]

runParser :: Text -> Parser a -> Maybe a
runParser i' =
  go (dropSpaces i') >=> \(o, a) -> if isEof o then Just a else Nothing
  where
    go :: Text -> Parser a -> Maybe (Text, a)
    go _ Fail = Nothing
    go i (Or pa pb) = go i pa <|> go i pb
    go i (App pf px) = do
      (o, f) <- go i pf
      fmap f <$> go o px
    go i (Map f px) = fmap f <$> go i px
    go i (Plus p) = plus i p
    go i (Between p (n :| ns)) = between i p (n : ns)

    plus :: Text -> Parser a -> Maybe (Text, NonEmpty a)
    plus i p = case star i p of
      (_, [])     -> Nothing
      (o, x : xs) -> Just (o, x :| xs)

    star :: Text -> Parser a -> (Text, [a])
    star i p = case go i p of
      Nothing     -> (i, [])
      Just (o, x) -> fmap (x :) (star o p)

    between :: Text -> Parser a -> [NamePart] -> Maybe (Text, [a])
    between _ _ []       = Nothing
    between i _ [n]      = (, []) <$> matchName i n
    between i p (n : ns) = do
      o <- matchName i n
      (o', x) <- go o p
      fmap (x :) <$> between o' p ns

    matchName :: Text -> NamePart -> Maybe Text
    matchName i (NamePart n) = dropSpaces <$> Text.stripPrefix n i

    dropSpaces :: Text -> Text
    dropSpaces = Text.dropWhile isSpace

    isEof :: Text -> Bool
    isEof = Text.null
