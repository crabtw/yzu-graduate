module Main where

import Control.Monad
import Data.List
import Data.Maybe

import qualified Data.ByteString.Lazy.UTF8 as U
import Network.FastCGI

import Lib.Interpreter
import Lib.Test

infoToText ((cur, min), pairs) = list $ creditMsg ++ allMsgs
    where creditMsg = item $ if min > cur
                                then "不足 " ++ setColor "red" (show (min - cur)) ++ " 學分"
                                else "學分符合最低標準"
          allMsgs = if null msgs then item "修習課程已達畢業條件" else concatMap item msgs
            where msgs = map pairMsgs $ filter ((> 0) . fst) pairs
          pairMsgs (remain, r) = "缺少 " ++ setColor "red" (show remain) ++ " 門" ++ ruleMsg r
          ruleMsg (Mult e _) = ruleMsg e
          ruleMsg (OneOf es) = list $ concatMap (item . (++"或") . ruleMsg) es
          ruleMsg (App s l) = cname ++ " 為 " ++ setColor "purple" (show l) ++ " 的課程"
            where cname = fromJust $ lookup s [("type", setColor "blue" "選別"),
                                               ("id", setColor "teal" "課號"),
                                               ("name", setColor "sienna" "課名")]

setColor c s = "<span style=\"color:" ++ c ++ "\">" ++ s ++ "</span>"
item s = "<li>" ++ s ++ "</li>"
list s = "<ul>" ++ s ++ "</ul>"

cgiMain (uid:pwd:rules:_) = do
    result <- liftIO $ test uid pwd rules
    case result of
        LoginError -> output "login failed"
        ParseError e -> output e
        Done info -> outputFPS $ U.fromString $ infoToText info

main = runFastCGI $ do
    setHeader "Content-type" "text/plain; charset=utf-8"
    method <- requestMethod
    vars <- liftM catMaybes $ mapM (getInputFPS) ["uid", "pwd", "rules"]
    if method /= "POST" || null vars
        then output "Nothing"
        else cgiMain (map U.toString vars)
