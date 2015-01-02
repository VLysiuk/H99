{--- https://www.haskell.org/haskellwiki/99_questions/21_to_28 ---}
import System.Random

{----#21----}
{-- Insert an element at a given position into a list. --}
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = let (left, right) = splitAt(i - 1) xs in left ++ x:right

{----#22----}
{-- Create a list containing all integers within a given range. --}
range :: Int -> Int -> [Int]
range i k
		| i <= k = i : range (i+1) k
		| otherwise = []

-- using syntactic sugar
range' :: Int -> Int -> [Int]
range' i k = [i .. k]

{----#23----}
{--Extract a given number of randomly selected elements from a list.--}
rnd_select :: [a] -> Int -> IO[a]
rnd_select [] _ = return []
rnd_select list n = do
	  			gen <- newStdGen
	  			let indexes = take n $ randomRs (0, length list - 1) gen
	  				in return $ map (\x -> list !! x) indexes


