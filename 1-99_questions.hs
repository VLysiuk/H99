lastElement :: (Ord a) => [a] -> a
lastElement []  = error "no elemets"
lastElement [a] = a
lastElement (x:xs) = lastElement xs 

lastButOne :: (Ord a) => [a] -> a
lastButOne [] = error "no elements"
lastButOne [x] = error "no elements"
lastButOne [x, y] = x
lastButOne (x:xs) = lastButOne xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "list is empty"
elementAt list i
				| i < 1 = error "invalid index"
				| otherwise = findByIndex list i 1

findByIndex :: [a] -> Int -> Int -> a
findByIndex [] _ _ = error "cannot find the element"
findByIndex (x:xs) i c
					| i == c = x
					| otherwise = findByIndex xs i (c + 1)

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "invalid"
elementAt' (x:_) 1 = x
elementAt' (_:xs) i 
				| i < 1 = error "invalid index"
				| otherwise = elementAt' xs (i-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = error "List is empty"
isPalindrome' [x] = True
isPalindrome' list = firstHalf == reverse secondHalf
				where
					firstHalf = take half list
					secondHalf = take half $ drop half' list
					half' = length (list) - half 
					half = length (list) `div` 2

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List xs) = foldr (\x acc -> (flatten x) ++ acc) [] xs
