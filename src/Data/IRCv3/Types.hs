{-# LANGUAGE DeriveGeneric #-}

module Data.IRCv3.Types where

import Data.Text (Text)
import GHC.Generics (Generic)


type Tag = (Text, Text)
type Tags = [Tag]

type Source = Text

type Command = Text

type Parameters = ([Text], Maybe Text)


data Message
  = Message
  { tags :: Maybe Tags
  , source :: Maybe Source
  , command :: Command
  , parameters :: Parameters }
  deriving (Generic, Show)
