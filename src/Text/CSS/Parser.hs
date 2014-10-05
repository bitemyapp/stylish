{-# LANGUAGE OverloadedStrings #-}
module Text.CSS.Parser
  ( AST(..)
  , Rule(..)
  , RuleSet(..)
  , runCssParser ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative((<*), (*>))

type Selector = String

-- General types
data Rule     = Rule String String deriving ( Show, Eq )
data RuleSet  = RuleSet [Rule] deriving ( Show, Eq )

-- CSS AST
data AST =
    Comment
  | Form Selector RuleSet
  deriving ( Show, Eq )

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

-- runCss :: Parser (Maybe AST)
runCssParser = cssParser $ lexeme (many1 parseCss)
