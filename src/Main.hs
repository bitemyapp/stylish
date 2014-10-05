{-# LANGUAGE OverloadedStrings #-}

module Main () where

import Data.Monoid
import Text.CSS.Parser as Parse
import qualified Data.Text as T
import Control.Applicative((<$>), (<*>))
import Network.HTTP

getExample :: FilePath -> IO T.Text
getExample name = T.pack `fmap` readFile fullPath >>= return
    where fullPath = "examples/" `mappend` name

runExample ::
    FilePath ->
    IO (Maybe [AST])
runExample e = Parse.runCssParser <$> cssString
    where cssString = T.unpack <$> getExample e

runEither e = Parse.runCssParseEither <$> cssString
    where cssString = T.unpack <$> getExample e

a = runExample "comments.css"
b = runExample "simple.css"
c = runExample "multiple.css"

-- Download bootstrap.css and run the verification steps over this for a sanity check
getBootStrap :: IO String
getBootStrap = simpleHTTP (getRequest file) >>= getResponseBody
    where file = "http://getbootstrap.com/dist/css/bootstrap.min.css"

-- A good overall test that the parser can handle anything
runBootstrap = Parse.runCssParseEither <$> getBootStrap
