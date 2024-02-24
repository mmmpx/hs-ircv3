{-# LANGUAGE DeriveGeneric #-}

module Data.IRCv3.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)


type Tag = (ByteString, ByteString)
type Tags = [Tag]

type Source = ByteString

type Command = ByteString

type Parameters = ([ByteString], Maybe ByteString)


data Message
  = Message
  { tags :: Maybe Tags
  , source :: Maybe Source
  , command :: Command
  , parameters :: Parameters }
  deriving (Generic, Show)
