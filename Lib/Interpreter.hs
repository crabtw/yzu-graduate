{-# LANGUAGE RecordWildCards #-}

module Lib.Interpreter where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

import Lib.Parser

import Debug.Trace

data Lit = Str String
         | UInt Int

instance Show Lit where
    show (Str s) = "\"" ++ s ++ "\""
    show (UInt i) = show i

data Expr = Mult Expr Lit
          | OneOf [Expr]
          | App String Lit

instance Show Expr where
    show (Mult e l) = subE ++ " * " ++ show l
        where subE = case e of
                        OneOf _ -> "(" ++ show e ++ ")"
                        _ -> show e
    show (OneOf es) = intercalate " | " (map show es)
    show (App s l) = s ++ "(" ++ show l ++ ")"

funcs = [("type", filterByType),
         ("id", filterById),
         ("name", filterByName)]

filterByType (Str s) = (s ==) . cosType
filterById (Str s) = (s `isPrefixOf`) . cosId
filterByName (Str s) = (s `isInfixOf`) . cosName

interpret cos ((UInt min), rs) = ((curCredit, min), test cos rs)
    where curCredit = foldl (flip $ (+) . cosCredit) 0 cos
          test cs (r:rs) = (remain, r) : test rest rs
            where (remain, rest) = intRule cs r
          test _ [] = []

intRule cos (OneOf es) = go cos es
    where go cs (e:es) = if remain == 0
                            then result
                            else if null es then (remain, rest) else go rest es
            where result@(remain, rest) = intRule cs e

intRule cos (Mult e (UInt i)) = go (0, cos) (i+1)
    where go (remain, cs) t = if t /= 0 && remain == 0
                                then go (intRule cs e) (t-1)
                                else (t, cs)

intRule cos (App s a) = if null sat
                            then (1, notSat)
                            else (0,  tail sat ++ notSat)
    where func = fromJust $ lookup s funcs
          (sat, notSat) = partition (func a) cos

parseRules = parse pExprs "rules"
    where TokenParser {..} = haskell
          pExprs = do
            whiteSpace
            min <- reserved "min-credit" >> pLit
            rs <- pOneOf `sepBy` (symbol ",")
            eof
            return (min, rs)
          pMult = do
            e <- parens pOneOf <|> pApp
            reservedOp "*"
            t <- pLit
            return $ Mult e t
          pOneOf = do
            es <- ((try pMult <|> pApp) `sepBy` (symbol "|"));
            if null es then fail ""  else return $ OneOf es
          pApp = liftM2 App (choice $ map (symbol . fst) funcs) (parens pLit)
          pLit = fmap Str stringLiteral <|> fmap (UInt . fromInteger) natural
