module Lecture01 where

-- Nearly all the problems that you see here are from 99 problems in haskell
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- NOTE: Do not copy the solutions from this page IF YOU WANT TO LEARN.
-- Otherwise well go ahead. Even better modify the test suite :P.
--
-- NOTE:
-- 1. Everything in Prelude is fair game to use.
-- 2. Try to solve the problems yourself.
-- 3. Only ask for help from people who attenend the lecture with you.
--    (Will create a channel where we can have descussions on these questions)
-- 4. Don't search for solutions on Google.

-- Problem 1
-- (*) Find the last element of a list.
-- λ> problem1 ['x','y','z']
-- 'z'
problem1 :: [a] -> a
problem1 [] = error "Cant work"
problem1 [x] = x
problem1 (x:xs) = problem1 xs


-- Problem 2
-- (*) Find the last but one element of a list.
-- λ> problem2 [1,2,3,4]
-- 3
-- λ> problem2 ['a'..'z']
-- 'y'
problem2 :: [a] -> a
problem2 [] = error "Cant work"
problem2 [x] = error "Cant work"
problem2 [x,y] = x
problem2 (x:xs) = problem2 xs


-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- Example:
-- λ> problem3 [1,2,3] 2
-- 2
-- λ> problem3 "haskell" 5
-- 'e'
problem3 :: [a] -> Int -> a
problem3 (x:xs) y
	| y == 1 = x
	| otherwise = problem3 xs (y-1)



-- Problem 4
-- (*) Find the number of elements of a list.
-- Example
-- λ> problem4 [123, 456, 789]
-- 3
-- λ> problem4 "Hello, world!"
-- 13
problem4 :: [a] -> Int
problem4 a = length a


-- Problem 5
-- (*) Reverse a list.
-- Example
-- λ> problem5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> problem5 [1,2,3,4]
-- [4,3,2,1]
problem5 :: [a] -> [a]
problem5 [] = []
problem5 (x:xs) = problem5 xs ++ [x]


-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or
--     backward; e.g. (x a m a x).
-- Example
-- λ> problem6 [1,2,3]
-- False
-- λ> problem6 "madamimadam"
-- True
-- λ> problem6 [1,2,4,8,16,8,4,2,1]
-- True
problem6 :: Eq a => [a] -> Bool
problem6 (x:xs) = reverse xs ++ [x] == [x] ++ xs 



-- TODO ------------------------------- (based on question asked)
-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
--      If a list contains repeated elements they should be replaced with a single copy
--      of the element. The order of the elements should not be changed.
-- Example:
-- λ> problem8 "aaaabccaadeeee"
-- "abcade"
problem8 :: Eq a => [a] -> [a]
problem8 [] = []
problem8 [x] = [x]
problem8 (x:xs) 
	| x == head xs = problem8 xs
	| otherwise = [x] ++ problem8 xs




-- Problem 14
-- (*) Duplicate the elements of a list.
--
-- Example:
-- λ> problem14 [1, 2, 3]
-- [1,1,2,2,3,3]
problem14 :: [a] -> [a]
problem14 [x] = [x,x]
problem14 (x:xs) = [x, x] ++ problem14 xs


-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
-- λ> problem15 "abc" 3
-- "aaabbbccc"
problem15 :: [a] -> Int -> [a]
problem15 [x] n = replicate n x
problem15 (x:xs) n = problem15 [x] n ++ problem15 xs n




-- Problem 18
-- (**) Extract a slice from a list.
--      Given two indices, i and k, the slice is the list containing the elements
--      between the i'th and k'th element of the original list (both limits included).
--      Start counting the elements with 1.
--
-- Example:
-- λ> problem18 ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
problem18 :: [a] -> Int -> Int -> [a]
problem18 [] i k = []
problem18 (x:xs) i k
	| i <= 1 && k >= 1 = [x] ++ problem18 xs (i-1) (k-1)
	| otherwise = problem18 xs (i-1) (k-1)


-- TODO ------------------------------- (based on question asked)
-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains
--      repeated elements they should be placed in separate sublists.
-- Example:
-- λ> problem9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
problem9 :: Eq a => [a] -> [[a]]
problem9 [x] = [[x]]
problem9 (x:xs)
 	| x == head xs = [[x] ++ head (problem9 xs)] ++ tail (problem9 xs)
	| otherwise = [[x]] ++ problem9 xs



-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the
--     so-called run-length encoding data compression method. Consecutive duplicates
--     of elements are encoded as lists (N E) where N is the number of duplicates of
--     the element E.
-- Example:
-- λ> problem10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
problem10 :: Eq a => [a] -> [(Int, a)]
problem10 [x] = [(1, x)]
problem10 (x:xs)
	| x == head xs = [(fst (head (problem10 xs)) + 1, x)] ++ tail (problem10 xs)
	| otherwise = [(1, x)] ++ problem10 xs 


-- Problem 20
-- (*) Remove the K'th element from a list.
--
-- Example:
-- λ> problem20 2 "abcd"
-- ('b',"acd")
problem20 :: Int -> [a] -> (a, [a])
problem20 n [x]  =  (x, [x])
problem20 n (x:xs) 
	| n > 1 || n < 1 =  (fst (problem20 (n-1) xs), [x] ++ snd (problem20 (n-1) xs) )
	| n == 1 = (x , snd (problem20 (n-1) xs))


-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
--     Do not use any predefined predicates.
--
-- Example:
-- λ> problem17 "abcdefghik" 3
-- ("abc", "defghik")
problem17 :: [a] -> Int -> ([a], [a])
problem17 [] n = ([], [])
problem17 (x:xs) n
	| n == 1 = ([x], xs)
	| n > 1 = ([x] ++ (fst (problem17 xs (n-1))), snd (problem17 xs (n-1)))


----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------



-- TODO ------------------------------- (No clue)
-- Problem 7
-- (**) Flatten a nested list structure.
--      Transform a list, possibly holding lists as elements into a `flat' list by
--      replacing each list with its elements (recursively).
-- Example:
-- Example in Haskell:
-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a
  = Elem a
  | List [NestedList a]
-- λ> problem7 (Elem 5)
-- [5]
-- λ> problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> problem7 (List [])
-- []

problem7 :: NestedList a -> [a]
problem7 = undefined





-- TODO ------------------------------- (based on question asked)
-- Problem 11
-- (*) Modified run-length encoding.
--     Modify the result of problem 10 in such a way that if an element has no
--     duplicates it is simply copied into the result list. Only elements with
--     duplicates are transferred as (N E) lists.
--
-- Example in lisp (Where it kinda makes sense):
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell (where it does not, not so much without IO):
--
-- λ> problem11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

-- Modified run-length encoding.
problem11 :: Eq a => [a] -> [ListItem a]
problem11 = undefined

-- TODO ------------------------------- (based on question asked)
-- Problem 12
-- Decode a run-length encoded list.
-- (**) Decode a run-length encoded list.
--      Given a run-length code list generated asspecified in problem 11.
--      Construct its uncompressed version.
--
-- Example:
-- λ> problem12 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd']
-- "aaaabccaad"
problem12 :: Eq a => [ListItem a] -> [a]
problem12 = undefined

-- TODO ------------------------------- (based on question asked)
-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
--      Implement the so-called run-length encoding data compression method directly. I.e.
--      don't explicitly create the sublists containing the duplicates, as in problem 9,
--      but only count them. As in problem P11, simplify the result list by replacing the
--      singleton lists (1 X) by X.
--
-- Example:
-- λ> problem13 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
problem13 :: Eq a => [a] -> [ListItem a]
problem13 = undefined




-- TODO ------------------------------- (based on question asked)
-- Problem 16
-- (**) Drop every N'th element from a list.
--
-- Example:
-- λ> problem16 "abcdefghik" 3
-- "abdeghk"

counter :: [a] -> Int -> Int -> [a]
counter [] n m = []
counter [x] n m = [x]
counter (x:xs) n m
	| n == 1 = counter xs m m
	| otherwise = [x] ++ counter xs (n-1) m


problem16 :: [a] -> Int -> [a]
problem16 [] n = [] 
problem16 (x:xs) n = counter (x:xs) n n



 




-- Problem 19
-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
-- λ> problem19 ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> problem19 ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
problem19 :: [a] -> Int -> [a]
problem19 = undefined




-- Problem A
-- (*) Implement merge:
--     Given two sorted lists give out a sorted list
-- Example:
-- λ> problemA [1,3,4] [2,5,6]
-- [1,2,3,4,5,6]
problemA :: Ord a => [a] -> [a] -> [a]
problemA [] [] = []
problemA (x:xs) (y:ys)
	| x < y = [x] ++ problemA (xs) (y:ys)
	| x > y = [y] ++ problemA (x:xs) (ys)



-- Problem B
-- (*) Implement merge-sort
--     Given a list returna  sorted list using merge sort algorithm you can use the merge
--     from previous problem
-- Example:
-- λ> problemB [4,3,2,1]
-- [1,2,3,4]
problemB :: Ord a => [a] -> [a]
problemB = undefined



