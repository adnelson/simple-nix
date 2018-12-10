{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Text.Render (
    Render(..), Indenter,
    indented, wrapIndented, inNewLine, renderIndented,
    renderIndentedStartingAt,
    renderTicks
  ) where

import ClassyPrelude
import qualified Prelude as P
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..), (<=<), (>=>), ask,
                             asks, runReaderT)
import Control.Monad.Writer (WriterT(..), MonadWriter(..), runWriterT)
import Control.Monad.State.Strict (MonadState, StateT, State, get, gets,
                                   modify, put, liftM, liftIO, runState,
                                   runStateT, execState, execStateT,
                                   evalState, evalStateT)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (ParseError)

type Name = Text

-- | A class for pretty printing, and in general, for "showing" as a `Text`.
class Show a => Render a where
  -- | Render the object as a `Text`.
  render :: a -> Text
  render = T.pack . P.show

  -- | Many types of objects need to be rendered in parentheses.
  renderParens :: a -> Text
  renderParens = render

  -- | Render in the `IO` monad. Useful for objects containing IORefs.
  renderIO :: MonadIO m => a -> m Text
  renderIO = return . render

  renderI :: a -> Indenter
  renderI = tell . render

instance Render Int
instance Render Bool
instance Render Integer
instance Render Double
instance Render Text
instance Render ParseError
instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <> render a <> ", " <> render b <> ")"
instance Render a => Render [a] where
  render list = "[" <> T.intercalate ", " (map render list) <> "]"

-- | Renders and surrounds in backticks. Useful for printing user input.
renderTicks :: Render a => a -> Text
renderTicks x = "`" <> render x <> "`"

type Indenter = ReaderT Int (WriterT Text (State Int)) ()

indented :: Indenter -> Indenter
indented action = do
  c <- get
  put (c+1)
  action
  put c

inNewLine :: Indenter -> Indenter
inNewLine action = do
  ilevel <- ask
  current <- get
  tell $ "\n" <> T.replicate (ilevel * current) " "
  action

wrapIndented :: Render a => Text -> Text -> [a] -> Indenter
wrapIndented start finish things = do
  tell start
  indented $ mapM_ (inNewLine . renderI) things
  inNewLine $ tell finish

renderIndented :: Render a => Int -> a -> Text
renderIndented = renderIndentedStartingAt 0

renderIndentedStartingAt :: Render a => Int -> Int -> a -> Text
renderIndentedStartingAt start level e =
  snd $ evalState (runWriterT (runReaderT (renderI e) level)) start
