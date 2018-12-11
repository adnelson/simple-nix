{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Virtu.Parser where

import Text.Parsec hiding (many, (<|>), spaces, parse, State, uncons)

import Nix.Common
import Nix.Parser (Parser, keyword, pIdentifier, schar)

newtype Identifier = Identifier Text
  deriving (Show, Eq, Generic)

data ImportedValue
  = Type Identifier
  | Val Identifier
  | Class Identifier
  | Everything
  deriving (Show, Eq, Generic)

type ImportedValues = [(ImportedValue, Maybe Identifier)]
newtype ModuleIdentifier = ModuleIdentifier [Identifier]
  deriving (Show, Eq, Generic)

data ImportClause = ImportClause {
  fromModule :: ModuleIdentifier,
  importedValues :: ImportedValues
  }
  deriving (Show, Eq, Generic)

data Module = Module {
  imports :: [ImportClause]
  }
  deriving (Show, Eq, Generic)

--- High level parsing types. These tell you, independent of exactly what AST is being produced, what syntactic device is currently being parsed (e.g. that a human would think of)
data SyntaxType
  = Parens
  | Curlies
  | CommaSeparatedItems
  | ColonSeparatedItems
  | ExpressionWithOperators
--- etc. Then, have a stack of these things when parsing.


-- NTS: consider mistakes that a user might make when entering the
-- code. Where possible, make those mistakes easy to avoid, and
-- otherwise make the error as obvious as possible.

-- TODO maybe should care whether it starts with a capital or not?
parseIdentifier :: Parser Identifier
parseIdentifier = Identifier <$> pIdentifier

parseImportedValue :: Parser ImportedValue
parseImportedValue = choice [
  map Type (keyword "type" *> parseIdentifier),
  map Val (keyword "val" *> parseIdentifier),
  map Class (keyword "class" *> parseIdentifier),
  Everything <$ keyword "*"
  ]

parseModuleIdentifier :: Parser ModuleIdentifier
parseModuleIdentifier = ModuleIdentifier <$> parseIdentifier `sepBy1` schar '.'

parseAliasedImport :: Parser (ImportedValue, Maybe Identifier)
parseAliasedImport = do
  ival <- parseImportedValue
  alias <- optionMaybe $ keyword "as" *> parseIdentifier
  pure (ival, alias)

inParens :: Parser a -> Parser a
inParens = between (schar '(') (schar ')')

inCurlies :: Parser a -> Parser a
inCurlies = between (schar '{') (schar '}')

parseImportClause :: Parser ImportClause
parseImportClause = do
  mod <- parseModuleIdentifier
  imports <- inParens $ parseAliasedImport `sepEndBy1` schar ','
  pure $ ImportClause mod imports

parseImports :: Parser [ImportClause]
parseImports = inCurlies $ parseImportClause `sepEndBy` schar ';'

parseModule :: Parser Module
parseModule = do
  keyword "module"
  inCurlies $ do
    imports <- keyword "import" *> parseImports
    pure $ Module imports
