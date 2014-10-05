{-# LANGUAGE OverloadedStrings #-}
module Main () where

import Data.Monoid
import Text.CSS.Parser as Parse
import qualified Data.Text as T
import Control.Applicative((<$>), (<*>))

getExample :: FilePath -> IO T.Text
getExample name = T.pack `fmap` readFile fullPath >>= return
    where fullPath = "examples/" `mappend` name

runExample ::
    FilePath ->
    IO (Maybe [AST])
runExample e = Parse.runCssParser <$> cssString
    where cssString = T.unpack <$> getExample e

a = runExample "comments.css"
b = runExample "simple.css"
