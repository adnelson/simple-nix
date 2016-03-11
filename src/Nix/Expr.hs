{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Nix.Expr where

import qualified Prelude as P
import Nix.Common
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T

data FuncArgs
  = Arg Name
  | Kwargs (HashMap Name (Maybe NixExpr)) Bool (Maybe Name)
  deriving (Show, Eq)

data NixExpr
  = Var Name
  | NixPathVar Name
  | Num Int
  | Bool Bool
  | Null
  | OneLineString NixString
  | MultiLineString NixString
  | Path FilePath
  | List [NixExpr]
  | Set Bool [NixAssign]
  | Let [NixAssign] NixExpr
  | Function FuncArgs NixExpr
  | Apply NixExpr NixExpr
  | With NixExpr NixExpr
  | If NixExpr NixExpr NixExpr
  | Dot NixExpr [NixString] (Maybe NixExpr)
  | BinOp NixExpr Text NixExpr
  | Not NixExpr
  | Assert NixExpr NixExpr
  deriving (Show, Eq)

data NixAssign
  = Assign [NixString] NixExpr
  | Inherit (Maybe NixExpr) (HashSet Name)
  deriving (Show, Eq)

data NixString
  = Plain Text
  | Antiquote NixString NixExpr NixString
  deriving (Show, Eq)

instance IsString NixString where
  fromString = Plain . fromString

instance IsString NixExpr where
  fromString = Var . fromString

(=$=) :: Name -> NixExpr -> NixAssign
k =$= v = Assign [Plain k] v

str :: Text -> NixExpr
str = OneLineString . Plain

dot :: NixExpr -> [NixString] -> NixExpr
dot e pth = Dot e pth Nothing

simpleKwargs :: [Name] -> FuncArgs
simpleKwargs ns = Kwargs (H.fromList $ map (\n -> (n, Nothing)) ns) False Nothing

simpleSet :: [(Name, NixExpr)] -> NixExpr
simpleSet = Set False . map (uncurry (=$=))

simpleSetRec :: [(Name, NixExpr)] -> NixExpr
simpleSetRec = Set True . map (uncurry (=$=))

-- | Shortcut for a simple kwarg set.
toKwargs :: [(Name, Maybe NixExpr)] -> FuncArgs
toKwargs stuff = Kwargs (H.fromList stuff) False Nothing

-- | Returns whether a string is a valid identifier.
isValidIdentifier :: Name -> Bool
isValidIdentifier "" = False
isValidIdentifier (unpack -> c:cs) = validFirst c && validRest cs
  where validFirst c = isAlpha c || c == '-' || c == '_'
        validRest (c:cs) = (validFirst c || isDigit c) && validRest cs
        validRest "" = True

-- | Renders a path.
renderPath :: [NixString] -> Text
renderPath = mapJoinBy "." ren where
  ren (Plain txt) | isValidIdentifier txt = txt
  ren txt = renderOneLineString txt

renderOneLineString :: NixString -> Text
renderOneLineString s = "\"" <> escape escapeSingle s <> "\""

renderMultiLineString :: NixString -> Text
renderMultiLineString s = "''" <> escape escapeMulti s <> "''"

renderParens e | isTerm e = render e
renderParens e = "(" <> render e <> ")"

renderKwargs :: [(Name, Maybe NixExpr)] -> Bool -> Text
renderKwargs ks dotdots = case (ks, dotdots) of
  ([], True) -> "{...}"
  ([], False) -> "{}"
  (ks, True) -> "{" <> ren ks <> ", ...}"
  (ks, False) -> "{" <> ren ks <> "}"
  where ren ks = mapJoinBy ", " ren' ks
        ren' (k, Nothing) = k
        ren' (k, Just e) = k <> " ? " <> render e

renderDot :: NixExpr -> [NixString] -> Maybe NixExpr -> Text
renderDot e pth alt = renderParens e <> rpth <> ralt where
  rpth = case pth of {[] -> ""; _ -> "." <> renderPath pth}
  ralt = case alt of {Nothing -> ""; Just e' -> " or " <> render e'}

-- | A "term" is something which does not need to be enclosed in
-- parentheses.
isTerm :: NixExpr -> Bool
isTerm (Var _) = True
isTerm (Num _) = True
isTerm (Bool _) = True
isTerm Null = True
isTerm (Path p) = True
isTerm (OneLineString _) = True
isTerm (MultiLineString _) = True
isTerm (List _) = True
isTerm (Set _ _) = True
isTerm (Dot _ _ Nothing) = True
isTerm (NixPathVar _) = True
isTerm _ = False

instance Render NixExpr where
  render = \case
    Var name -> name
    Num n -> pack $ show n
    Bool True -> "true"
    Bool False -> "false"
    Null -> "null"
    NixPathVar v -> "<" <> v <> ">"
    OneLineString s -> renderOneLineString s
    MultiLineString s -> renderMultiLineString s
    Path pth -> pathToText pth
    List es -> "[" <> mapJoinBy " " render es <> "]"
    Set True asns -> "rec " <> render (Set False asns)
    Set False asns -> "{" <> concatMap render asns <> "}"
    Let asns e -> concat ["let ", concatMap render asns, " in ",
                          render e]
    Function arg e -> render arg <> ": " <> render e
    Apply e1@(Apply _ _) e2 -> render e1 <> " " <> render e2
    Apply e1 e2 -> render e1 <> " " <> renderParens e2
    With e1 e2 -> "with " <> render e1 <> "; " <> render e2
    Assert e1 e2 -> "assert " <> render e1 <> "; " <> render e2
    If e1 e2 e3 -> "if " <> render e1 <> " then "
                         <> render e2 <> " else " <> render e3
    Dot e pth alt -> renderDot e pth alt
    BinOp e1 op e2 -> renderParens e1 <> " " <> op <> " " <> renderParens e2
    Not e -> "!" <> render e

  renderI expr = case expr of
    List es -> wrapIndented "[" "]" es
    Set True asns -> tell "rec " >> renderI (Set False asns)
    Set False asns -> wrapAssigns "{" "}" asns
    Let asns e -> wrapAssigns "let " "in " asns >> renderI e
    Function params e -> renderI params >> tell ": " >> renderI e
    Apply e1@(Apply _ _) e2 -> renderI e1 >> tell " " >> renderI e2
    Apply e1 e2 | isTerm e2 -> renderI e1 >> tell " " >> renderI e2
    Apply e1 e2 -> renderI e1 >> tell " (" >> renderI e2 >> tell ")"
    With e1 e2 -> do
      tell "with "
      renderI e1
      tell "; "
      renderI e2
    BinOp e1 op e2 -> do
      renderI e1
      tell $ " " <> op <> " "
      renderI e2
    e -> tell $ render e

renderSepBy :: Render a => Text -> [a] -> Indenter
renderSepBy sep [x, y] = renderI x >> tell sep >> renderI y
renderSepBy sep [x] = renderI x
renderSepBy sep (x:xs) = renderI x >> tell sep >> renderSepBy sep xs
renderSepBy _ [] = return ()

wrapAssigns :: Text -> Text -> [NixAssign] -> Indenter
wrapAssigns start finish [] = tell start >> tell finish
wrapAssigns start finish [a] = tell start >> renderI a >> tell finish
wrapAssigns start finish asns = wrapIndented start finish asns

instance Render FuncArgs where
  render (Arg a) = a
  render (Kwargs k dotdots mname) =
    let args = renderKwargs (H.toList k) dotdots
    in args <> maybe "" (\n -> " @ " <> n) mname

  renderI (Arg a) = tell a
  renderI k@(Kwargs ks _ _) | H.size ks <= 4 = tell $ render k
  renderI (Kwargs ks dotdots mname) = do
    tell "{"
    indented $ do
      let pairs = H.toList ks
          renderPair (n, v) = inNewLine $ do
            tell n
            case v of
              Nothing -> return ()
              Just e -> tell " ? " >> renderI e
          trailingCommas = if dotdots then pairs else P.init pairs
          final = if dotdots then Nothing else Just $ P.last pairs
      forM_ trailingCommas $ \(n, v) -> do
        renderPair (n, v)
        tell ","
      forM_ final renderPair
      when dotdots $ inNewLine $ tell "..."
    inNewLine $ tell "}"
    case mname of
      Nothing -> return ()
      Just name -> tell " @ " >> tell name

instance Render NixAssign where
  render (Assign p e) = renderPath p <> " = " <> render e <> ";"
  render (Inherit maybE names) = do
    let ns = joinBy " " $ HS.toList names
        e = maybe "" (\e -> " (" <> render e <> ") ") maybE
    "inherit " <> e <> ns <> ";"

  renderI (Assign p e) = do
    tell $ renderPath p <> " = "
    renderI e
    tell "; "
  renderI (Inherit maybE names) = do
    let ns = joinBy " " $ HS.toList names
        e = maybe "" (\e -> " (" <> render e <> ") ") maybE
    tell $ "inherit " <> e <> ns <> "; "

escapeSingle :: String -> String
escapeSingle s = case s of
  '$':'{':s' -> '\\':'$':'{':escapeSingle s'
  '\n':s' -> '\\':'n':escapeSingle s'
  '\t':s' -> '\\':'t':escapeSingle s'
  '\r':s' -> '\\':'r':escapeSingle s'
  '\b':s' -> '\\':'b':escapeSingle s'
  c:s' -> c : escapeSingle s'
  "" -> ""

escapeMulti :: String -> String
escapeMulti s = case s of
  '$':'{':s' -> '\\':'$':'{':escapeMulti s
  c:s' -> c : escapeMulti s'
  "" -> ""

escape :: (String -> String) -> NixString -> Text
escape esc (Plain s) = pack $ esc $ unpack s
escape esc (Antiquote s e s') = concat [escape esc s, "${", render e,
                                        "}", escape esc s']
