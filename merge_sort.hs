-- Import Read to use readMaybe.
import Text.Read

-- ****************** INPUT PARSING GOES HERE *******************

-- To safely consume floats.
parseInput :: String -> Maybe Float
parseInput x = if x == "sort" then Nothing else (readMaybe x) :: Maybe Float

-- Do some logging
-- like a good program.
logHeader :: IO()
logHeader = do
    putStrLn "Enter floats on new lines."
    putStrLn "Once done, feed empty line by hitting enter."
    putStrLn "This program will sort your inputs using Merge Sort and show a sorted list."
    

-- A typical driver.
getInputs :: IO [Float]
getInputs = do
    putStrLn "Enter a float (or feed empty line if done): "
    inp <- getLine
    case parseInput inp of
        Nothing -> return []
        Just goodNum -> do
            allInps <- getInputs
            return (goodNum:allInps)

-- ****************** INPUT PARSING ENDS HERE *******************

-- ****************** CORE LOGIC BEGINS HERE *************************

-- Define mergesort as a function that takes
-- a list of floats and outputs a sorted list
-- of floats.
mergesort :: Ord a => [a] -> [a]
-- Since it will be recursive, define the base cases:

-- Empty lists are already sorted!
mergesort [] = []

-- Lists of size one are also already sorted!
mergesort [a] = [a]

-- Lists of size two need one comparison.
mergesort [a, b] = if a > b then [b, a] else [a, b]

-- For larger lists, break them in half
-- and apply merge sort on them.
mergesort xs = 
    -- Find the mid point.
    -- split list by mid point and recursively sort them.
    -- after done sorting, combine them.
    combine (mergesort $ take mid xs) (mergesort $ drop mid xs)
        where mid = div (length xs) 2



-- Define combine as a function that takes
-- two lists and outputs their combined list.
combine :: Ord a => [a] -> [a] -> [a]
-- if a list is empty, the other is the result.
combine [] a = a
combine b [] = b
-- Suppose neither of them are empty.
-- then extract the first elements from
-- both the lists.
combine (first_a:a_rest) (first_b:b_rest) =
    -- In the combined list, pick the smaller of
    -- first_a, first_b and recurse on the remaining.
    if first_a > first_b
        then first_b : (combine (first_a:a_rest) b_rest)
    else first_a : (combine (first_b:b_rest) a_rest)

-- *************** CORE LOGIC ENDS HERE **************


main :: IO()
main = do
    logHeader
    allNums <- getInputs
    putStrLn "The numbers you entered are: "
    putStrLn $ show $ allNums
    putStrLn "After sorting:"
    putStrLn $ show $ mergesort allNums

