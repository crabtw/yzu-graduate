module Lib.Test where

import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment
import System.Exit

import qualified Data.ByteString.Lazy.UTF8 as B
import Network.Curl
import qualified System.IO.UTF8 as U

import Lib.Interpreter
import Lib.Parser

type Resp = CurlResponse_ [(String, String)] ByteString

data Result a = LoginError
              | ParseError String
              | Done a

url path = "https://portal.yzu.edu.tw" ++ path
logincheck_new = "/logincheck_new.asp"
student_main = "/personal/student/student_main.asp"
student_learn2 = "/personal/student/student_learn2.asp"

getCosPage h uid pwd = do
    let info = [CurlPost True, CurlPostFields ["uid="++uid, "pwd="++pwd]]
    do_curl_ h (url logincheck_new) info :: IO Resp
    do_curl_ h (url student_main) method_GET :: IO Resp
    resp <- do_curl_ h (url student_learn2) method_GET :: IO Resp
    return $ B.toString $ respBody resp

getCosInfo uid pwd = withCurlDo $ do
    h <- initialize
    setopts h [CurlCookieJar "/tmp/cookies"]
    page <- getCosPage h uid pwd
    let info = parsePage page
    return $ if null info then Nothing else Just info

test uid pwd rules = do
    info <- getCosInfo uid pwd
    case info of
        Nothing -> return LoginError
        Just rows -> do
            return $ either (ParseError . show) (Done . interpret rows) (parseRules rules)
