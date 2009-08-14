module Lib.Parser where

import Data.Char
import Data.Maybe

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Regex.TDFA

data Info = Info {
        cosType :: String,
        cosId :: String,
        cosName :: String,
        cosCredit :: Int,
        cosScore :: Int
    }

instance Show Info where
    show = cosId

toList = map (map rmSpaces . drop 2 . cols) . rows
    where cols = map (fromTagText . (!! 1)) . sections td
            where td = tagOpen (== "td") (const True)
          rows = map (takeWhile (not . trClose)) . sections trOpen . parseTags
            where trOpen = tagOpen (== "tr") (anyAttrName (== "class"))
                  trClose = tagClose (== "tr")
          rmSpaces s = sub
            where (_, sub, _) = match re s :: (String, String, String)
                  re = makeRegex "[^[:space:]]+" :: Regex

toInfo [t, i, n, c, s] = entry $ if isDigit $ head s then read s else 666
    where entry = Info t i n (read c)

parsePage = filter ((>= 60) . cosScore) . map toInfo . toList
