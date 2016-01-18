import Data.List 

--Using "map" to defines a list called "nat" that's equivalent to the predefined list [0..]. Do not use dots.
nat :: [Integer]	
nat = 0 : map (+1) nat

--Using list comprehenson, defines a list called "mult" containing all those non-negative integers that are 
--multiples of both 5 and 6. In your answer to this question do not use "map". 

--My function "mult" makes use of the function `mod` which i found at "http://zvon.org/other/haskell/Outputprelude/mod_f.html"
mult :: [Integer]	
mult = [ x | x <- [0..], x `mod` 5 == 0, x `mod` 6 == 0]  

--Defines a function "exists". Given a list lx of Integers and an Integer y, the function delivers True if y occurs in lx.
--Try to make your function work in as wide a class of situations (involving integers) as you can. As a comment to your function, 
--say as precisely as you can under what circumstances your function works as advertised (i.e., state its preconditions).

-- My function "exists" makes use of the function `elem` which I found at "http://zvon.org/other/haskell/Outputprelude/elem_f.html"

-- If checking for negative integers then it will need to be specified in parentheses e.g., exists [-1, 0, 1] (-1). 
-- This function will run infinitely if given an infinite set lx of Integers and an Integer y, where y does not exist in the list.
exists :: [Integer] -> Integer -> Bool
exists lx y = y `elem` lx

--Defines a function "atmostonce". Given a list of Integers lx and a number y, the function delivers True if y occurs at most once in lx 
--(in other words, y occurs zero or one times in lx). 

--My function "atmostonce" makes use of the functions:
--`length` which I found at "zvon.org/other/haskell/Outputprelude/length_f.html",
--`filter` which I found at "http://zvon.org/other/haskell/Outputprelude/filter_f.html"

--My atmostonce function will only work with finite sets.
--My atmostonce function will only work with Integer inputs.
--My atmostonce function will only work for negatives if number y is written in parentheses e.g., atmostonce [1,2,3] (-1) 
atmostonce :: [Integer] -> Integer -> Bool
atmostonce lx y 
	| length (filter (==y) lx) > 1 = False
	| otherwise = True

--Defines a list cart2 which evaluates to the sequence [(0,0), (1,0), (2,0), (3,0), etc.], 

cart2 :: [(Integer,Integer)]
cart2 = [(x,y) | x <- [0..], y <- [0]]

--Defines a list cart3 which evaluates to the sequence [(0,0), (0,1), (0,2), (0,3), etc.]

cart3 :: [(Integer,Integer)]
cart3 = [(x,y) | x <- [0], y <- [0..]]

--Defines a list cart4 which evaluates to the entire Cartesian product Int x Int
--"cart4" makes use of the functions: 
--`zip` which I found at "http://learnyouahaskell.com/starting-out",
--`concatMap` which I found at "http://zvon.org/other/haskell/Outputprelude/concatMap_f.html",
--`nub` which I found at "http://stackoverflow.com/questions/19389065/remove-duplicates-from-haskell-list"
cart4 :: [(Integer,Integer)]
cart4 = nub(concatMap(\(x,y) -> [x,y]) (zip  [(x,y) | x <- [0..], y <- [0..x]] [(x,y) | y <- [0..], x <- [0..y]]))

--Defines a funct s such that s n is the set of type [(Int,Int)], containing all pairs (x,y) such that x+y=n.
--My sn function will only work for a non-negative input
sn :: Integer -> [(Integer, Integer)]
sn n = [(x,y) | x <- [0..n], y <- [0..n], x+y == n]
