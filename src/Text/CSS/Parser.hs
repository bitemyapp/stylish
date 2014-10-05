{-# LANGUAGE OverloadedStrings #-}
module Text.CSS.Parser
  ( AST(..), css ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative((<*), (*>))
type Selector = String

data Rule     = Rule String String deriving Show
data RuleSet  = RuleSet [Rule] deriving Show

data AST =
    Comment
  | Form Selector RuleSet
  deriving ( Show )

-- | Returns a parser that skips whitespace on both sides
lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

rule :: Parser Rule
rule = do
    -- Need to define a proper parser for the declarations
    k <- many1 (letter <|> (oneOf "-"))
    char ':' >> spaces
    v <- many1 (noneOf ";")
    char ';' -- todo this needs to be optional on the last form!
    return $ Rule k v

parseRules :: Parser [Rule]
parseRules = many $ lexeme rule

ruleSet :: Parser RuleSet
ruleSet = do
    char '{'
    rules <- spaces >> parseRules
    char '}'
    return $ RuleSet rules

form :: Parser AST
form = do
    selector <- many1 (noneOf " ")
    spaces
    rules    <- ruleSet
    return $ Form selector rules

comment :: Parser AST
comment = do
    string "/*"
    spaces
    manyTill anyChar (string "*/")
    spaces
    return Comment

-- parseHexCode :: Parser AST
-- parseHexCode = do
--    char '#'

parseCss :: Parser AST
parseCss = comment <|> form

cssParser :: Parser a -> String -> Maybe a
cssParser parser input = case parse parser "css" input of
                           Left _  -> Nothing
                           Right v -> Just v

cssString = "div { font-size: blue; color: rgba(255,255,255); }"

css = cssParser (spaces *> many1 parseCss <* spaces)
