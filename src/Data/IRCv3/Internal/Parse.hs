module Data.IRCv3.Internal.Parse where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import Data.IRCv3.Types
import Control.Applicative

(<++>) :: P.Parser B.ByteString -> P.Parser B.ByteString -> P.Parser B.ByteString
a <++> b = B.append <$> a <*> b

bs :: String -> P.Parser B.ByteString
bs = P.string . C.pack

pack1 :: Char -> B.ByteString
pack1 = C.pack . singleton

pack2 :: Char -> Char -> B.ByteString
pack2 a b = C.pack [a,b]

pack3 :: Char -> Char -> Char -> B.ByteString
pack3 a b c = C.pack [a,b,c]

pcount :: Int -> P.Parser B.ByteString -> P.Parser B.ByteString
pcount n p = cn $ P.count n p

btw :: Char -> Char -> P.Parser Char
btw a b = P.satisfy (\c -> c >= a && c <= b)

upto :: Int -> P.Parser a -> P.Parser [a]
upto n p | n > 0 = (:) <$> p <*> upto (n-1) p <|> return []
upto _ _ = return []

upto1 :: Int -> P.Parser a -> P.Parser [a]
upto1 n p | n > 0 = (:) <$> p <*> upto (n-1) p
upto1 _ _ = return []

cn :: P.Parser [B.ByteString] -> P.Parser B.ByteString
cn p = B.concat <$> p

orempty :: P.Parser B.ByteString -> P.Parser B.ByteString
orempty = P.option B.empty

hexDigit :: P.Parser Char
hexDigit = P.satisfy isHexDigit

-- ABNF

servername :: P.Parser B.ByteString
servername = hostname

host :: P.Parser B.ByteString
host = hostname <|> hostaddr

hostname :: P.Parser B.ByteString
hostname = shortname <++> (cn $ many dsh)
  where
    dsh = (bs ".") <++> shortname

shortname :: P.Parser B.ByteString
shortname = lod <++> (cn $ many lodh) <++> (cn $ many lod)
  where
    lod = pack1 <$> (P.letter_ascii <|> P.digit)
    lodh = lod <|> (bs "-")

hostaddr :: P.Parser B.ByteString
hostaddr = ip4addr <|> ip6addr

ip4addr :: P.Parser B.ByteString
ip4addr = octet <++> dot <++> octet <++> dot <++> octet <++> dot <++> octet
  where
    octet =     (pack1 <$> P.digit                                  ) -- 0-9
            <|> (pack2 <$> btw '1' '9' <*> P.digit                  ) -- 10-99
            <|> (pack3 <$> P.char '1' <*> P.digit <*> P.digit       ) -- 100-199
            <|> (pack3 <$> P.char '2' <*> btw '0' '4' <*> P.digit   ) -- 200-249
            <|> (pack3 <$> P.char '2' <*> P.char '5' <*> btw '0' '5') -- 250-255
    dot = bs "."

ip6addr :: P.Parser B.ByteString
ip6addr =     (                     pcount 6 h16c <++> ls32)
          <|> (           dcol <++> pcount 5 h16c <++> ls32)
          <|> (pre 0 <++> dcol <++> pcount 4 h16c <++> ls32)
          <|> (pre 1 <++> dcol <++> pcount 3 h16c <++> ls32)
          <|> (pre 2 <++> dcol <++> pcount 2 h16c <++> ls32)
          <|> (pre 3 <++> dcol <++> h16c          <++> ls32)
          <|> (pre 4 <++> dcol                    <++> ls32)
          <|> (pre 5 <++> dcol                    <++> h16 )
          <|> (pre 6 <++> dcol                             )
  where
    h16 = C.pack <$> upto1 4 hexDigit
    h16c = C.append <$> h16 <*> (P.string $ C.pack ":")
    ls32 = (h16 <++> col <++> h16) <|> ip4addr
    col = bs ":"
    dcol = bs "::"
    pre 0 = orempty h16
    pre n = orempty ((cn $ upto (n-1) h16c) <++> h16)

tag :: P.Parser Tag
tag = (,) <$> key <*> (P.char '=' *> val)
  where
    key :: P.Parser B.ByteString
    key = orempty (bs "+") <++> orempty (vendor <++> bs "/") <++> P.takeWhile1 isKeyChar
    val :: P.Parser B.ByteString
    val = P.takeWhile1 (not . endOfVal)
    vendor :: P.Parser B.ByteString
    vendor = host
    isKeyChar :: Char -> Bool
    isKeyChar c = or $ map ($ c) [P.isAlpha_ascii, P.isDigit, (== '-')]
    endOfVal :: Char -> Bool
    endOfVal = (flip elem) "\0\r\n; "

