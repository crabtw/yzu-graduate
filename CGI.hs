module Main where

import Control.Monad
import Data.List
import Data.Maybe

import qualified Data.ByteString.Lazy.UTF8 as U
import Network.FastCGI

import Lib.Interpreter
import Lib.Test

infoToText ((cur, min), pairs) = intercalate "\n" $ creditMsg : (map pairMsgs $ filter ((> 0) . fst) pairs)
    where creditMsg = if min > cur then "不足 " ++ show (min - cur) ++ " 學分" else "學分足夠"
          pairMsgs (remain, r) = "缺少 " ++ show remain ++ " 門" ++ ruleMsg r
          ruleMsg (Mult e _) = case e of
                                OneOf _ -> "，" ++ ruleMsg e
                                _ -> ruleMsg e
          ruleMsg (OneOf es) = intercalate "或" (map ruleMsg es)
          ruleMsg (App s l) = cname ++ "為 " ++ show l ++ " 的課程"
            where cname = fromJust $ lookup s [("type", "選別"), ("id", "課號"), ("name", "課名")]

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
