--  				Assignment 2 
-- 				By Justin McNamara
-- 					 CSCI 3330


-- #1  
-- a) after2 xs = tail(tail xs)
--     [a] -> [a]

-- b) dropwhile p xs
--		| xs == [] = []
--		| p(head xs) = dropwhile p(tail xs)
--		| otherwise = xs
-- (Eq a) => (a -> Bool) ->[a]->[a]

-- c) makel(x,y)=[x,y]
-- (a,a) -> [a]

-- d) palindrome xs = reverse xs == xs
-- Eq a =>[a]->Bool

-- e) doublevals xs = map double xs
--			where double x = x+x
-- Num a=>[a]->[a]



-- #2 define deleteAll
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll p (x:xs)
	| xs == []		=	[]
	| p == x		=	deleteAll p xs
	| otherwise		=	x: deleteAll p xs
	


-- #3 Define interleave1 & interleave2
interleave1 :: Int -> [Int] -> [Int]
interleave1 y xs = concat[[y,x] | x <- xs]

interleave2 :: Int -> [Int] -> [Int]
interleave2 y xs = interleave1 y xs ++ [y]



-- #4 Define explode
-- part i
explode:: Int -> Int -> [Int]
explode n x
	| n < 1		=		[]
	| otherwise	= 		x:explode(n-1)x


-- part ii
explodelist:: Int -> Int -> Int -> [[Int]]	
explodelist k n x
	| k < 1		=		[]
	| otherwise	=		(explode n x):explodelist(k-1) n x
		
		
-- #5 Define merge
merge :: Eq a => [a] -> [a] -> [a]
merge xs ys
	| xs == []	= 	ys
	| ys == []	=	xs
	| otherwise 	=	[head xs]++[head ys]++merge(tail xs)(tail ys)

-- #6 Define totalLength
totalLength :: Eq a => [[a]] -> Int
totalLength xss
	| xss == [] 		=	0
	| otherwise		=	length(head xss) + totalLength(tail xss)


