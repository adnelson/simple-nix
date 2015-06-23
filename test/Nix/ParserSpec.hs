{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Nix.ParserSpec (main, spec) where

import SpecHelper
import Nix.Expr
import Nix.Parser
import qualified Data.HashMap.Strict as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parsing" $ do
  variablesSpec
  primitivesSpec
  dotSpec
  functionsSpec
--  letSpec
--  recordsSpec
  applicationSpec
  ifSpec
  binOpSpec
  listsSpec

lambda :: Name -> NixExpr -> NixExpr
lambda n = Function (Arg n)

variablesSpec :: Spec
variablesSpec = describe "variables" $ do
  it "should parse variables" $ do
    parseNix "x" `shouldBeR` "x"
    parseNix "foo-bar" `shouldBeR` "foo-bar"
    parseNix "_foo_bar" `shouldBeR` "_foo_bar"
    parseNix "FooBar" `shouldBeR` "FooBar"
    parseNix "x$y" `shouldHaveErr` ""

primitivesSpec :: Spec
primitivesSpec = describe "primitives" $ do
  it "should parse primitives" $ do
    parseNix "3" `shouldBeR` Num 3
    parseNix "\"hello\"" `shouldBeR` OneLineString (Plain "hello")
    parseNix "true" `shouldBeR` Bool True

dotSpec :: Spec
dotSpec = describe "dots" $ do
  it "should parse dots" $ do
    parseNix "a.foo" `shouldBeR` dot "a" [Plain "foo"]

  it "should parse more complex stuff with dots" $ do
    parseNix "(x + y).z" `shouldBeR` dot (BinOp "x" "+" "y") [Plain "z"]

  it "should parse multiple dots" $ do
    parseNix "a.b.c" `shouldBeR` dot "a" [Plain "b", Plain "c"]

functionsSpec :: Spec
functionsSpec = describe "functions" $ do
  describe "simple lambdas" $ do
    it "should parse a simple lambda" $ do
      parseNix "x: x" `shouldBeR` lambda "x" "x"

    it "should parse a nested lambda" $ do
      let output = lambda "x" $ lambda "y" $ Apply "y" "x"
      parseNix "x: y: y x" `shouldBeR` output

    it "should parse a lambda with a let statement" $ do
      let output = lambda "x" $ Let ["y" =$= "x"] "y"
      parseNix "x: let y = x; in y" `shouldBeR` output

    it "should parse a nested lambda with a let statement" $ do
      let output = lambda "x" $ Let ["y"=$="x"] (lambda "z" $ Apply "z" "y")
      parseNix "x: let y = x; in z: z y" `shouldBeR` output

  describe "lambdas with kwargs" $ do
    it "should parse a lambda with kwargs" $ do
      let output = Function (simpleKwargs ["x", "y"]) $ Apply "x" "y"
      parseNix "{x, y}: x y" `shouldBeR` output

    it "should get default arguments" $ do
      let ks = H.fromList [("x", Nothing), ("y", Just "x"), ("z", Nothing)]
          output = Function (Kwargs ks False Nothing) (Num 1)
          input = "{x, y ? x, z}: 1"
      parseNix input `shouldBeR` output

    it "should parse variadic functions" $ do
      let ks = H.fromList [("x", Nothing)]
          output = Function (Kwargs ks True Nothing) (Num 1)
          input = "{x, ...}: 1"
      parseNix input `shouldBeR` output

    it "should get purely variadic functions" $ do
      let output = Function (Kwargs mempty True Nothing) (Num 1)
          input = "{...}: 1"
      parseNix input `shouldBeR` output

    it "should let us name the arguments" $ do
      let ks = H.fromList [("x", Nothing), ("y", Nothing)]
          output = Function (Kwargs ks True (Just "z")) (Num 1)
          input = "{x, y, ...}@z: 1"
      parseNix input `shouldBeR` output

-- letSpec :: Spec
-- letSpec = describe "let statements" $ do
--   it "should parse a let statement" $ do
--     parseNix "let foo = 1; foo" `shouldBeR` Let "foo" (Int 1) "foo"

--   it "should parse a nested let statement" $ do
--     let output = Let "x" "y" $ Let "z" "w" $ Int 3
--     parseNix "let x = y; let z = w; 3" `shouldBeR` output

--   it "should parse function declarations" $ do
--     let output = Let "f" (Lambda "x" (binary "x" "+" (Int 3))) "f"
--     parseNix "let f x = x + 3; f" `shouldBeR` output

--   it "should parse function declarations with multiple args" $ do
--     let output = Let "f" (Lambda "x" (Lambda "y" (binary "x" "+" "y"))) "f"
--     parseNix "let f x y = x + y; f" `shouldBeR` output

--   it "should parse function declarations with patterns" $ do
--     let output = Let "f" (Lambda "a" (Case "a" [(Int 1, Int 2)])) "f"
--     parseNix "let f 1 = 2; f" `shouldBeR` output

--   it "should parse function declarations with multiple patterns" $ do
--     let body = Case "a" [(Int 1, Int 2), ("y", binary "y" "+" (Int 3))]
--     let output = Let "f" (Lambda "a" body) "f"
--     parseNix "let f 1 = 2 | f y = y + 3; f" `shouldBeR` output

--   it "should parse function declarations with multiple args/patterns" $ do
--     let input = "let f 1 2 = 0 | f x y = x + y; f"
--     -- This desugars to:
--     -- let f a b = if [a, b] is [1, 2] -> 0 | [x, y] -> [x + y]; f
--         body = Case ["a", "b"] [([Int 1, Int 2], Int 0),
--                                 (["x", "y"], binary "x" "+" "y")]
--         output = Let "f" (Lambda "a" $ Lambda "b" body) "f"
--     parseNix input `shouldBeR` output

--   it "should parse symbol function declarations" $ do
--     let input = "let x +-+ y = x * (y + x); 0"
--         output = Let "+-+" (Lambda "x" $ Lambda "y" $
--                              binary "x" "*" (binary "y" "+" "x")) (Int 0)
--     parseNix input `shouldBeR` output

--   it "should parse symbol function declarations with multiple patterns" $ do
--     let input = "let 1 +-+ 0 = 1 | x +-+ y = x * y; 0"
--         body = Case ["a", "b"] [([Int 1, Int 0], Int 1),
--                                 (["x", "y"], binary "x" "*" "y")]
--         output = Let "+-+" (Lambda "a" $ Lambda "b" body) (Int 0)
--     parseNix input `shouldBeR` output

--   it "should fail if the wrong function name is used" $ do
--     let input = "let foo 0 = 1 | bar 1 = 2; baz"
--     parseNix input `shouldHaveErr` "Expected function named \"foo\""

--   it "should fail if the wrong number of arguments is used" $ do
--     let input = "let foo 0 = 1 | foo 1 2 = 3; baz"
--     parseNix input `shouldHaveErr` "Wrong number of arguments, expected 1"

--   it "should fail if adding patterns to function with all variables" $ do
--     let input = "let foo x = 1 | foo y = 2; foo"
--     parseNix input `shouldHaveErr` "unexpected"
--     let input = "let foo = 1 | foo = 2; foo"
--     parseNix input `shouldHaveErr` "unexpected"

setsSpec :: Spec
setsSpec = describe "sets" $ do
  it "should parse sets" $ do
    let set = simpleSet [("foo", Num 2), ("bar", Num 3)]
    parseNix "{foo=2; bar=3}" `shouldBeR` set

  it "should parse recursive sets" $ do
    let set = simpleSetRec [("foo", Num 2), ("bar", Num 3)]
    parseNix "rec {foo=2; bar=3}" `shouldBeR` set

  it "should parse nested sets" $ do
    let rec = simpleSet [("a", "a"), ("b", Num 1),
                         ("foo", simpleSet [("c", Num 12)])]
    parseNix "{a=a; b=1; foo={c=12;};}" `shouldBeR` rec

applicationSpec :: Spec
applicationSpec = describe "applications" $ do
  it "should parse an application with variables" $ do
    parseNix "x y" `shouldBeR` Apply "x" "y"

  it "should parse an application with constants" $ do
    parseNix "f True" `shouldBeR` Apply "f" "True"
    parseNix "x 1" `shouldBeR` Apply "x" (Num 1)

  it "should nest applications to the left" $ do
    parseNix "a b c" `shouldBeR` Apply (Apply "a" "b") "c"

  it "should be able to apply things in parentheses" $ do
    parseNix "(a b) c" `shouldBeR` Apply (Apply "a" "b") "c"
    parseNix "a (b c)" `shouldBeR` Apply "a" (Apply "b" "c")

ifSpec :: Spec
ifSpec = describe "if statements" $ do
  it "should parse if statements" $ do
    let output = If (Bool False) (Num 1) (Num 2)
    parseNix "if false then 1 else 2" `shouldBeR` output

  it "should parse nested if statements" $ do
    let output = If (Bool True) (If (Bool False) "a" "b") "c"
    parseNix "if true then if false then a else b else c" `shouldBeR` output
    let output = If (Bool True) "a" (If (Bool False) "b" "c")
    parseNix "if true then a else if false then b else c" `shouldBeR` output
    let output = If (If (Bool True) "a" "b") "c" "d"
    parseNix "if if true then a else b then c else d" `shouldBeR` output

binOpSpec :: Spec
binOpSpec = describe "binary operations" $ do
  let plus a b = BinOp a "+" b
  it "should do addition" $ do
    parseNix "a + b" `shouldBeR` plus "a" "b"
  it "should do nested addition" $ do
    parseNix "a + b + c" `shouldBeR` plus (plus "a" "b") "c"
  let minus a b = BinOp a "-" b
  it "should do subtraction" $ do
    parseNix "a - b" `shouldBeR` minus "a" "b"
  it "should do nested subtraction" $ do
    parseNix "a - b - c" `shouldBeR` minus (minus "a" "b") "c"
  it "should respect parentheses" $ do
    parseNix "a - (b - c)" `shouldBeR` minus "a" (minus "b" "c")

  it "should not take key symbols as PlusOp operators" $ do
    parseNix "a = b" `shouldHaveErr` ""

listsSpec :: Spec
listsSpec = describe "list literals" $ do
  it "should do list literals" $ do
    parseNix "[a b c]" `shouldBeR` List [Var "a", Var "b", Var "c"]

  it "should do nested list literals" $ do
    parseNix "[a [b c] d]" `shouldBeR` List [Var "a",
                                                List [Var "b", Var "c"],
                                                Var "d"]
