{------------------------------- p01 ------------------------------
(*) Find the last element of a list.

Example in Haskell:
λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
-------------------------------------------------------------------}
p01 :: [a] -> a
p01 [] = error "Empty list"
p01 [x] = x
p01 (_:xs) = p01 xs
p01' = head . reverse

--- test    
test01 :: [Bool]
test01 = [ 
         p01[1,2,3,4] == 4, 
         p01['x','y','z'] == 'z'
         ]

{------------------------------- p02 ------------------------------
(*) Find the last but one element of a list.

Example in Haskell:
λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
-------------------------------------------------------------------}

p02 :: [a] -> a
p02 [] = error "Empty list"
p02 x = reverse x !! 1

p02' = last . init -- pipe is backward. init -> last

p02'' [x, _] = x
p02'' (_:xs) = p02'' xs

--- test
test02 :: [Bool]
test02 = [p02'' [1, 2, 3, 4] == 3,
          p02'' ['x', 'y', 'z'] == 'y'
         ]

{------------------------------- p03 ------------------------------
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'

-------------------------------------------------------------------}
p03 :: [a] -> Int -> a
p03 x k = x !! (k-1)

--- test
test03 :: [Bool]
test03 = [
        p03 [1,2,3] 2 == 2,
        p03 "haskell" 5 == 'e'
    ]

{------------------------------- p04 ------------------------------
(*) Find the number of elements of a list.

Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-------------------------------------------------------------------}
p04 :: [a] -> Int
p04 [] = 0
p04 (x:xs) =  1 + p04 xs
test04 = [p04 [1, 2, 3] == 3,
          p04 "Hello, world!" == 13]


--- main
runTest :: [Bool] -> String
runTest [] = "No test cases"
runTest x = if all (id) x
               then "Test succeeded " ++ show x
               else "Test failed " ++ show x
testcases = [test01, test02, test03, test04]
main = do
    putStr $ unlines $ map runTest (testcases)
