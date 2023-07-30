module Data.IRCv3.Types where

import qualified Data.ByteString as BS

type Tag = (BS.ByteString, BS.ByteString)
type Tags = [Tag]

type Source = BS.ByteString

type Command = BS.ByteString

type Parameters = ([BS.ByteString], Maybe BS.ByteString)

data Message
  = Message
  { tags :: Maybe Tags
  , source :: Maybe Source
  , command :: Command
  , parameters :: Parameters }
  deriving Show

