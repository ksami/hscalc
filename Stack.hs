module Stack
( Stack(Stack)
, push
, pop
, empty
) where


data Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push a (Stack b) = Stack (a : b)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)