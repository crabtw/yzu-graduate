module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment

import qualified System.IO.UTF8 as U

import Lib.Interpreter
import Lib.Test

infoToText ((cur, min), pairs) =  creditMsg ++ allMsgs
    where creditMsg = if min > cur
                        then "不足 " ++ setColor "35" (show (min - cur)) ++ " 學分\n\n"
                        else "學分符合最低標準\n\n"
          allMsgs = if null msgs then "修習課程已達畢業條件" else intercalate "\n" msgs
            where msgs = map pairMsgs $ filter ((> 0) . fst) pairs
          pairMsgs (remain, r) = "缺少 " ++ setColor "35" (show remain) ++ " 門\n\t" ++ ruleMsg r
          ruleMsg (Mult e _) = ruleMsg e
          ruleMsg (OneOf es) = intercalate "或\n\t" (map ruleMsg es)
          ruleMsg (App s l) = cname ++ "為 " ++ setColor "34" (show l) ++ " 的課程"
            where cname = fromJust $ lookup s [("type", setColor "31" " 選別 "),
                                               ("id", setColor "32" " 課號 "),
                                               ("name", setColor "33" " 課名 ")]

setColor c s = "\ESC[" ++ c ++ "m" ++ s ++ "\ESC[0m"

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
