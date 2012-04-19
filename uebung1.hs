{-
	Uebung 1
	Abgabe von:
	Sebastian Schmitz 282491
	Jonas Fortmann ------
-}



-- 1a) i)
add :: Int -> Int -> Int
add x y = x+y

-- ii)
add2 :: [Int] -> [Int] -> Int 
add2 (x:xs) (y:ys) = x + y

{-
alternativ:
length3 :: [Int] -> [Int] -> Int 
length3 x y = length2 x + length2 y

length2 :: [Int] -> Int
length2 [] = 0
length2 (x:xs) = length[xs]+1
-}

-- 1b)
-- Bool -> Int -> Int

-- 2
-- a)  [x,y] -> x,y besitzen den gleichen Typ. (x:y) damit nicht möglich
-- b) x = Int, y = [Int], z = [Int]
-- c) x muss links liste sein und rechts element
-- d) y muss rechts element sein und links liste dieser elemente
-- e) x Int, y Int
-- f) x Int, y Int

-- 3
-- a)
isPowerOfTwo :: Int -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo 1 = True
isPowerOfTwo x = if(even x) then isPowerOfTwo (div x 2) else False

-- b)
filterPowersOfTwo :: [Int] -> [Int]
filterPowersOfTwo [] = []
filterPowersOfTwo (x:xs) |isPowerOfTwo x = x:filterPowersOfTwo xs
			|otherwise 	= filterPowersOfTwo xs

-- c)
sort :: [Int] -> [Int]
sort [] = []
sort x = maximum x : sort (delete x (maximum x))

delete :: [Int] -> Int -> [Int]
delete [] y = []
delete (x:xs) y | x == y = xs
		| otherwise = x: delete xs y

-- d)
-- i) Zwei Mal, einmal um das maximale Element nach vorne zu stellen und dann um eben dieses aus der Liste zu löschen.
-- ii)
sort2 :: [Int] -> [Int]
sort2 [] = []
sort2 x = y : sort (delete x y)
	where y = maximum x

-- 4)
infixr 6 >-
(>-) :: [[a]] -> [a] -> [a]
[] >- z = []
(x:xs) >-  y = ( x ++ y) ++ ( xs >- y ) 

{- Eingaben: ["a","b","c"] >- ["1"] >- "!"
		["a","b","c"] >- (["1"] >- "!") ergibt jeweils "a1!b1!c1!"
	
	["a","b","c"] >- "1" ++ "!"
	(["a","b","c"] >- "1") ++ "!" ergibt jeweils "a1b1c1!"
-}