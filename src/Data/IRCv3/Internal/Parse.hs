{-# LANGUAGE OverloadedStrings #-}

module Data.IRCv3.Internal.Parse
  ( message ) where

import Data.Attoparsec.Text ( Parser )
import qualified Data.Attoparsec.Text as AP
import Data.Char ( isAlpha, isDigit, isHexDigit )
import Data.IRCv3.Types
  ( Message(Message)
  , Tags
  , Tag
  , Source
  , Command
  , Parameters )
import qualified Data.List as List
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Text.Builder.Linear ( Builder, fromChar, fromText, runBuilder)
import Control.Applicative ( Alternative((<|>)) )

sb :: Text -> Parser Builder
sb x = fromText <$> AP.string x

cb' :: Functor f => f Char -> f Builder
cb' p = fromChar <$> p

cb :: Char -> Parser Builder
cb x = fromChar <$> AP.char x

(<++>) :: Applicative f => f Text -> f Text -> f Builder
a <++> b = (<>) <$> (fromText <$> a) <*> (fromText <$> b)

(<-+>) :: Applicative f => f Builder -> f Text -> f Builder
a <-+> b = (<>) <$> a <*> (fromText <$> b)

(<+->) :: Applicative f => f Text -> f Builder -> f Builder
a <+-> b = (<>) <$> (fromText <$> a) <*> b

(<-->) :: (Applicative f, Semigroup b) => f b -> f b -> f b
a <--> b = (<>) <$> a <*> b

build :: Builder -> Text
build = runBuilder

cn :: Foldable t => t Builder -> Builder
cn = List.foldl' (<>) (fromText "")

pcount :: Monad m => Int -> m Builder -> m Builder
pcount n p = cn <$> AP.count n p

btw :: Char -> Char -> Parser Builder
btw a b = fromChar <$> AP.satisfy (\c -> c >= a && c <= b)

upto :: Int -> Parser a -> Parser [a]
upto n p | n > 0 = (:) <$> p <*> upto (n-1) p <|> return []
upto _ _ = return []

upto1 :: Int -> Parser a -> Parser [a]
upto1 n p | n > 0 = (:) <$> p <*> upto (n-1) p
upto1 _ _ = return []

orempty :: Parser Builder -> Parser Builder
orempty = AP.option (fromText "")

hexDigit :: Parser Char
hexDigit = AP.satisfy isHexDigit

maybep :: Parser a -> Parser (Maybe a)
maybep p = AP.option Nothing (Just <$> p)

crlf :: Parser Text
crlf = AP.string "\r\n"

-- -- ABNF

message :: Parser Message
message =     Message
          <$> maybep tags'
          <*> maybep source'
          <*> command
          <*> parameters
          <*  crlf
  where
    tags' = AP.char '@' *> tags <* AP.char ' '
    source' = AP.char ':' *> source <* AP.char ' '

source :: Parser Source
source =     build
         <$> (
                   (nick <--> orempty (cb '!' <-+> user) <--> orempty (cb '@' <--> host))
               <|> servername
             )
  where
    nick =      (Text.singleton <$> AP.satisfy (not . (flip elem ("\0\r\n# " :: String))))
           <++> AP.takeWhile (not . (flip elem ("\0\r\n " :: String)))
    user = AP.takeWhile1 (not . (flip elem ("\0\r\n " :: String)))

command :: Parser Command
command = AP.takeWhile1 isAlpha <|> (Text.pack <$> AP.count 3 AP.digit)

parameters :: Parser Parameters
parameters =     (,)
             <$> AP.many' (AP.char ' ' *> middle)
             <*> maybep (AP.char ' ' *> AP.char ':' *> trailing)
  where
    middle = AP.takeWhile1 (not . (flip elem ("\0\r\n: " :: String)))
    trailing = AP.takeWhile1 (not . (flip elem ("\0\r\n" :: String)))

servername :: Parser Builder
servername = hostname

host :: Parser Builder
host = hostname <|> hostaddr

hostname :: Parser Builder
hostname = shortname <--> (cn <$> AP.many' dsh)
  where
    dsh = (cb '.') <--> shortname

shortname :: Parser Builder
shortname = lod <--> (cn <$> AP.many' lodh) <--> (cn <$> AP.many' lod)
  where
    lod = cb' (AP.letter <|> AP.digit)
    lodh = lod <|> (cb '-')

hostaddr :: Parser Builder
hostaddr = ip4addr <|> ip6addr

ip4addr :: Parser Builder
ip4addr = octet <--> (dot *> octet) <--> (dot *> octet) <--> (dot *> octet)
  where
    octet =     (                              digit       ) -- 0-9
            <|> (             btw '1' '9' <--> digit       ) -- 10-99
            <|> ( cb '1' <--> digit       <--> digit       ) -- 100-199
            <|> ( cb '2' <--> btw '0' '4' <--> digit       ) -- 200-249
            <|> ( cb '2' <--> cb '5'      <--> btw '0' '5' ) -- 250-255
    digit = cb' AP.digit
    dot = AP.char '.'

ip6addr :: Parser Builder
ip6addr =     (                      pcount 6 h16c <--> ls32 )
          <|> (            dcol <--> pcount 5 h16c <--> ls32 )
          <|> ( pre 0 <--> dcol <--> pcount 4 h16c <--> ls32 )
          <|> ( pre 1 <--> dcol <--> pcount 3 h16c <--> ls32 )
          <|> ( pre 2 <--> dcol <--> pcount 2 h16c <--> ls32 )
          <|> ( pre 3 <--> dcol <--> h16c          <--> ls32 )
          <|> ( pre 4 <--> dcol                    <--> ls32 )
          <|> ( pre 5 <--> dcol                    <--> h16  )
          <|> ( pre 6 <--> dcol                              )
  where
    col   = cb ':'
    dcol  = sb "::"
    h16   = fromText <$> Text.pack <$> upto1 4 hexDigit
    h16c  = h16 <--> col
    ls32  = (h16c <--> h16) <|> ip4addr
    pre 0 = orempty h16
    pre n = orempty ((cn <$> upto (n-1) h16c) <--> h16)

tags :: Parser Tags
tags =     (:)
       <$> tag
       <*> AP.many' (AP.char ';' *> tag)

tag :: Parser Tag
tag =     (,)
      <$> key
      <*> (AP.char '=' *> val)
  where
    key = build <$> orempty (cb '+') <--> orempty (vendor <--> cb '/') <-+> AP.takeWhile1 isKeyChar
    vendor = host
    isKeyChar c = or $ map ($ c) [isAlpha, isDigit, (== '-')]
    val = AP.takeWhile (not . endOfVal)
    endOfVal = flip elem ("\0\r\n; " :: String)
