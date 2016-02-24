import Lexer
import Stack


test :: String
test = do
    let e = flex "5 2 + 3 1 - *"
    showStack $ parse e empty

showStack :: Maybe (Stack String) -> String
showStack Nothing = "Invalid expression"
showStack (Just (Stack [])) = ""
showStack (Just s) = (showElem ele) ++ (showStack (Just s1))
    where (ele, s1) = pop s

showElem :: Maybe String -> String
showElem Nothing = "Trying to pop empty stack"
showElem (Just a) = a

parse :: Expr -> Stack String -> Maybe (Stack String)
parse [] _ = Nothing
parse (Nothing:xs) _ = Nothing
parse (Just x:xs) s
    | t == "Num" = parse xs (push a s)
    | t == "Op" =
        let res = case a of "+" -> (+)
                            "-" -> (-)
                            otherwise -> (*)
        in parse xs $ push (toString ((fmap res (toInt a2)) <*> (toInt a1))) s2
    | t == "End" = Just s
    | otherwise = Nothing
    where (t,a) = x
          (a1, s1) = pop s
          (a2, s2) = pop s1

toInt :: Maybe String -> Maybe Int
toInt Nothing = Nothing
toInt (Just "") = Nothing
toInt (Just n) = Just (read n :: Int)

toString :: Maybe Int -> String
toString Nothing = "0"
toString (Just n) = show n