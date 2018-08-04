module Transpiler where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace

alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

digit :: String
digit = ['0' .. '9']

tokenize :: String -> (String, String)
tokenize [] = ("", "")
tokenize xxs@(c : cs)
  | c == '-' && head cs == '>' = ("->", tail cs)
  | c `elem` "(){}," = ([c], cs)
  | not (null s) = (s, ss)
  | otherwise = tokenize cs
  where
    (s, ss) = span (`elem` alpha ++ digit) xxs

isName :: String -> Bool
isName "" = False
isName (c:cs) = elem c alpha && all (`elem` alpha ++ digit) cs  

isNumber' :: String -> Bool
isNumber' "" = False
isNumber' s = all (`elem` digit) s

isNameOrNumber :: String -> Bool
isNameOrNumber s = isName s || isNumber' s 

isComma :: String -> Bool
isComma s = s == ","

-----------------------------------------------------
-------------- your parser combinator ---------------
-----------------------------------------------------
type ParserFun val = String -> [(val, String)]
newtype Parser val = Parser { parse :: ParserFun val }

parseCode :: Parser a -> String -> Either String a
parseCode m s = case parse m s of
  [(res, rest)] -> case tokenize rest of
    ("", "") -> Right res
    _ -> Left "Hugh?"
  _           -> Left "Hugh?"

data Fun  = Fun Expr [Expr] (Maybe Expr)
          deriving Show
data Expr = Nam String
          | Lam [Expr] [Expr]
          deriving Show

formatF (Fun expr params (Just lambda)) =
  formatF (Fun expr (params ++ [lambda]) Nothing)
formatF (Fun expr params Nothing) =
  (formatE expr) ++ "(" ++ (formatP params) ++ ")"

formatE (Nam name) =
  name
formatE (Lam lparams lstmts) =
  "(" ++ (formatP lparams) ++ "){" ++ (formatS lstmts) ++ "}"


formatP [] = ""
formatP (p:ps) = formatE p ++ (formatP' ps)
  where
    formatP' [] = ""
    formatP' (p:ps) = "," ++ (formatE p) ++ (formatP' ps)

formatS [] = ""
formatS (x:xs) = formatE x ++ ";" ++ (formatS xs)

functionParser = Parser parser
  where
    parser input = parser1 input <|> parser2 input
    parser1 input =
      let
        val = parse expressionParser input
        val' = feed val (enl . tokenize)
        val'' = feed val' $ parse parametersParser
      in
       case feed val'' (enl . tokenize) of
        [((((expr,"("),params),")"), rest)] ->
          case parse lambdaParser rest of
           [] -> [(Fun expr params Nothing, rest)]
           [(lambda, rest')] -> [(Fun expr params (Just lambda), rest')]
        _ -> []
    parser2 input = 
      let
        val = parse expressionParser input
      in
       case feed val $ parse lambdaParser of
        [((expr, lambda), rest)] -> [(Fun expr [] (Just lambda), rest)]
        _ -> []
        

parametersParser = Parser $ parser []
  where
    parser acc input =
      let
        val = parse expressionParser input
      in
       case feed val (enl . tokenize) of
        [((param, ","), rest)] -> parser' (param:acc) rest
        [((param, other), rest)] -> [(reverse (param:acc), other ++ rest)]
        [] -> [([], input)]
    parser' acc input =
      let
        val = parse expressionParser input
      in
       case feed val (enl . tokenize) of
        [((param, ","), rest)] -> parser' (param:acc) rest
        [((param, other), rest)] -> [(reverse (param:acc), other ++ rest)]
        [] -> []
        
nameParser = Parser parser
  where
    parser input = [(Nam token, rest) | isNameOrNumber token] 
      where
        (token, rest) = tokenize input

expressionParser :: Parser Expr
expressionParser = Parser parser
  where
    parser input = parse nameParser input <|> parse lambdaParser input

lambdaParser = Parser parser
  where
    parser input = 
      let
        val = enl . tokenize $ input
        val' = feed val parser'
        val'' = feed val' $ parse lambdaStmtsParser
      in
       case feed val'' (enl . tokenize) of
        [(((("{", params), stmts),"}"), rest)] -> [(Lam params stmts, rest)]
        _ -> []
    parser' input =
      let
        val = parse lambdaParamsParser input
      in
       case feed val (enl . tokenize) of
        [(([], "->"), rest)] -> []
        [((params, "->"), rest)] -> [(params, rest)]
        [] -> []
        _ -> [([], input)]

lambdaParamsParser = Parser $ parser []
  where
    parser acc input =
      let
        val = parse lambdaParamParser input
      in
       case feed val (enl . tokenize) of
        [((param, ","), rest)] -> parser' (param:acc) rest
        [((param, other), rest)] -> [(reverse (param:acc), other ++ rest)]
        [] -> [([], input)]
    parser' acc input =
      let
        val = parse lambdaParamParser input
      in
       case feed val (enl . tokenize) of
        [((param, ","), rest)] -> parser' (param:acc) rest
        [((param, other), rest)] -> [(reverse (param:acc), other ++ rest)]
        [] -> []

lambdaStmtsParser = Parser $ parser []
  where
    parser acc input = 
      case parse lambdaParamParser input of
       [(val, rest)] -> parser (val:acc) rest
       [] -> [(reverse acc, input)]
     
lambdaParamParser = Parser parser
  where
    parser input = [(Nam token, rest) | isNameOrNumber token] 
      where
        (token, rest) = tokenize input

feed :: [(a, String)] -> ParserFun b -> [((a,b), String)]
feed [] _ = []
feed [(val, rest)] parseFun = 
  case parseFun rest of
   [] -> []
   [(val', rest')] -> [((val, val'), rest')] 

enl :: a -> [a]
enl x = [x]

transpile :: String -> Either String String
transpile input =
  case parseCode functionParser input of
   Left string -> Left string
   Right val -> Right $ formatF val
