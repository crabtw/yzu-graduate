{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment

import qualified System.IO.UTF8 as U

import Lib.Interpreter
import Lib.Test

infoToText ((cur, min), pairs) = intercalate "\n" $ creditMsg : (map pairMsgs pairs)-- $ filter ((> 0) . fst) pairs)
    where creditMsg = if min > cur then "不足 " ++ show (min - cur) ++ " 學分" else "學分足夠"
          pairMsgs (remain, r) = "缺少 " ++ show remain ++ " 門" ++ ruleMsg r
          ruleMsg (Mult e _) = case e of
                                OneOf _ -> "，" ++ ruleMsg e
                                _ -> ruleMsg e
          ruleMsg (OneOf es) = intercalate "或" (map ruleMsg es)
          ruleMsg (App s l) = cname ++ "為 " ++ show l ++ " 的課程"
            where cname = fromJust $ lookup s [("type", "選別"),
                                               ("id", "課號"),
                                               ("name", "課名")]

run uid pwd rules = do
    result <- test uid pwd rules
    return $ case result of
                LoginError -> "login failed"
                ParseError e ->  e
                Done info -> infoToText info

main = do
    uid:pwd:file:_ <- getArgs
    rules <- U.readFile file
    U.putStrLn =<< run uid pwd rules
