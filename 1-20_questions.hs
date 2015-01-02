{- ***https://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems*** -}
import System.Random

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

{----#6-----}
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

{----#7-----}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List xs) = foldr (\x acc -> (flatten x) ++ acc) [] xs

{----#8-----}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

{----#9-----}
pack :: (Eq a)=>[a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack list@(x:xs) =  takeWhile(== x) list : pack(dropWhile (==x) list)

{----#10----}
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode list = map (\e -> (length e, head e)) $ pack list

{----#11----}
data Element a = Multiple Int a | Single a deriving (Show)
encodeModified :: Eq a => [a] -> [Element a]
encodeModified = map f . pack
					where f x = if length x > 1 then Multiple (length x) (head x)
								else Single (head x)

{----#12----}
decodeModified :: Eq a => [Element a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
							Single e 	 -> e : decodeModified xs
							Multiple c e -> replicate c e ++ decodeModified xs

{----#13----}
encodeDirect :: Eq a => [a] -> [Element a]
encodeDirect [] = []
encodeDirect list@(x:xs) = headElement : encodeDirect remainingList
							where
								headElement = if (count > 1) then Multiple count x else Single x
								(count, remainingList) = countHeadElement list x 0

countHeadElement :: Eq a => [a] -> a -> Int -> (Int, [a])
countHeadElement [] e c = (c, [])
countHeadElement list@(x:xs) e c
							| x == e = countHeadElement xs e (c + 1)
							| otherwise = (c, list)

{----#14----}
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

{----#15----}
repli :: [a] -> Int -> [a]
repli list c = concatMap(\x -> replicate c x ) list

{----#16----}
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list c = take (c-1) list ++ dropEvery (drop c list) c

{----#17----}
split :: [a] -> Int -> ([a], [a])
split list count = (take count list, drop count list)

-- recursive
split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' list count = (takeFirst list count 0, takeSecond list count 0)

takeFirst :: [a] -> Int -> Int -> [a]
takeFirst [] _ _ = []
takeFirst (x:xs) count current
						| current < count = x : takeFirst xs count (current + 1)
						| otherwise = []

takeSecond :: [a] -> Int -> Int -> [a]
takeSecond [] _ _ = []
takeSecond (x:xs) count current
						| current >= count = x : takeSecond xs count (current + 1)
						| otherwise = takeSecond	xs count (current + 1)

{----#18----}
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice list i k = foldr sliceFilter [] $ zip [1..] list
						where sliceFilter x xs =  if fst x >= i && fst x <= k then snd x : xs else xs

-- using list comprehensions
slice' :: [a] -> Int -> Int -> [a]
slice' list i k = [x | (indx, x) <- zip [1..] list, indx >= i && indx <= k]

{----#19----}
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list pos
			| pos < 0 = rotateRight list $ normalize pos
			| otherwise = rotateLeft list $ normalize pos
				where
					normalize p = abs p `mod` length list
					rotateRight xs p = drop (length xs - p) xs ++ take (length xs - p) xs
					rotateLeft xs p = drop p xs ++ take p xs

{----#20----}
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "list is empty"
removeAt i list
			| i < 1 || i > length list = error "index is out of range" 
 			| otherwise = (elem, remainings $ zip [1..] list)
							where
								elem = list !! (i - 1)
								remainings = foldr(\x acc -> if (fst x /= i) then snd x : acc else acc) []

-- point free but no error handling
removeAt' :: Int -> [a] -> (a, [a])
removeAt' n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)