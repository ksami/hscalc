module Lexer
( flex
, Token
, Expr
, printExpr
) where

import Data.Char

type Token = (String, String)
type Expr = [Maybe Token]

printExpr :: Expr -> String
printExpr [] = ""
printExpr (Just x:xs) =
    if snd x == "\0" then
        ""
    else
        snd x ++ ", " ++ printExpr xs
printExpr (Nothing:xs) = "InvalidToken, " ++ printExpr xs

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator _ = False

flex :: String -> Expr
flex "" = [Just ("End", "\0")]
flex all@(x:xs)
    | isNumber x =
        let (num, rest) = span (isNumber) all
        in Just ("Num", num) : flex rest
    | isOperator x = Just ("Op", [x]) : flex xs
    | isSpace x = flex xs
    | otherwise = Nothing : flex xs
