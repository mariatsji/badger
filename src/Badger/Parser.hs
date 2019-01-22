{-# LANGUAGE OverloadedStrings #-}

module Badger.Parser where

import           Control.Applicative

import qualified Data.Attoparsec.Text          as AT
import qualified Data.Text                     as T

import           Badger.Lang

parensParser :: AT.Parser Expr
parensParser =
    (AT.char '(' AT.<?> "open parens missing")
        *> exprParser
        <* (AT.char ')' AT.<?> "closing parens missing")

sumParser :: AT.Parser Expr
sumParser = do
    _ <- AT.string "sum" *> AT.space AT.<?> "sum operator missing"
    _ <- AT.space AT.<?> "Missing space in sum"
    a <- exprParser AT.<?> "Missing a, expected 'sum <a>'"
    Sum a <$> exprParser

valParser :: AT.Parser Expr
valParser =
    Val
        <$>    (AT.string "val" *> AT.space *> AT.double)
        AT.<?> "Unable to parse val"

exprParser :: AT.Parser Expr
exprParser =
    AT.space
        *>     parensParser
        <|>    sumParser
        <|>    valParser
        <*     AT.space
        AT.<?> "Expected 'val <num>', 'sum <num> <num>' or some compound expression using parens"

parse :: T.Text -> Either String Expr
parse = AT.parseOnly exprParser

parseString :: String -> Either String Expr
parseString = (AT.parseOnly exprParser) . T.pack