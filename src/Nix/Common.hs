{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Nix.Common (
    module ClassyPrelude,
    module Control.Applicative,
    module Control.Exception,
    module Control.Exception.ErrorList,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.Identity,
    module Control.Monad.State.Strict,
    module Control.Monad.Reader,
    module Control.Monad.Writer,
    module Control.Monad.Trans,
    module Data.Char,
    module Data.HashMap.Strict,
    module Data.Either,
    module Data.Maybe,
    module Data.List,
    module Data.String.Utils,
    module Filesystem.Path.CurrentOS,
    module GHC.Exts,
    module GHC.IO.Exception,
    module Text.Render,
    Name, Record,
    tuple, tuple3, fromRight, pathToText,
    putStrsLn, putStrs, dropSuffix, maybeIf,
    joinBy, mapJoinBy
  ) where

import ClassyPrelude hiding (assert, asList, find, FilePath, bracket,
                             maximum, maximumBy, try)
import qualified Prelude as P
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), (<=<), (>=>), ask,
                             asks, runReaderT)
import Control.Monad.Writer (WriterT(..), MonadWriter(..), runWriterT)
import Control.Monad.State.Strict (MonadState, StateT, State, get, gets,
                                   modify, put, liftM, liftIO, runState,
                                   runStateT, execState, execStateT,
                                   evalState, evalStateT)
import Control.Monad.Except (ExceptT, MonadError(..), throwError, runExceptT)
import Control.Exception.ErrorList
import Control.Monad.Identity (Identity(..))
import Control.Applicative hiding (empty, optional)
import Data.Char (isDigit, isAlpha)
import Data.List (maximum, maximumBy)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Either (isRight, isLeft)
import Data.String.Utils hiding (join)
import qualified Data.Text as T
import GHC.Exts (IsList)
import GHC.IO.Exception
import Control.Exception (bracket)
import Text.Render hiding (renderParens)
import Filesystem.Path.CurrentOS (FilePath, fromText, toText, collapse)

-- | Indicates that the text is some identifier.
type Name = Text

-- | A record is a lookup table with string keys.
type Record = HashMap Name

-- | Takes two applicative actions and returns their result as a 2-tuple.
tuple :: Applicative f => f a -> f b -> f (a, b)
tuple action1 action2 = (,) <$> action1 <*> action2

-- | Takes three applicative actions and returns their result as a 3-tuple.
tuple3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
tuple3 action1 action2 action3 = (,,) <$> action1 <*> action2 <*> action3

-- | Creates a new hashmap by applying a function to every key in it.
alterKeys :: (Eq k, Hashable k, Eq k', Hashable k') =>
             (k -> k') -> HashMap k v -> HashMap k' v
alterKeys f mp = do
  let pairs = H.toList mp
  let newPairs = P.map (\(k, v) -> (f k, v)) pairs
  let newMap = H.fromList newPairs
  newMap

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error "Expected `Right` value"

putStrsLn :: MonadIO m => [Text] -> m ()
putStrsLn = putStrLn . concat

putStrs :: MonadIO m => [Text] -> m ()
putStrs = putStr . concat

dropSuffix :: String -> String -> String
dropSuffix suffix s | s == suffix = ""
dropSuffix suffix (c:cs) = c : dropSuffix suffix cs
dropSuffix suffix "" = ""

maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing

grab :: (Hashable k, Eq k) => k -> HashMap k v -> v
grab k = fromJust . H.lookup k

joinBy :: Text -> [Text] -> Text
joinBy = T.intercalate

mapJoinBy :: Text -> (a -> Text) -> [a] -> Text
mapJoinBy sep func = joinBy sep . map func

pathToText :: FilePath -> Text
pathToText pth = case toText pth of
  Left p -> p
  Right p -> p
