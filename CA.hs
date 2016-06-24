{- 
 
Programmer : Neil Babson
November 25, 2015
CS 557
Portland State

-}


import System.Random
import System.IO
import PPM6
import Control.Monad

rule30 :: [Int]
rule30 = [0,0,0,1,1,1,1,0]

-- Given an integer seed, generate an infinite list of random 0s and 1s
randNums :: Int -> [Int]
randNums gen = randomRs (0,1) (mkStdGen gen)

-- Given an integer seed, generate an infinite list of random Doubles between 0 and 1
randNums' :: Int -> [Double]
randNums' gen = randomRs (0,1) (mkStdGen gen)

-- Gets an intger from the user
getInt :: IO Int
getInt = do line <- getLine
            if (head line < '0' || head line > '9')
              then do
                putStr "Invalid input.  Try again: "
                getInt
            else do   
              return (read line :: Int)

-- Gets a Boolean value from the user   
getBool = do ans <- getLine
             if head ans /= 'n' && head ans /='N' && head ans /= 'y' && head ans /= 'Y' then do
               putStr "Invalid input.  Try again: "
               getBool
             else do 
               if head ans == 'y' || head ans == 'Y' then return True
               else return False

-- Identify CAs that settle into repeating pattern or pattern that shifts one space right or left 
-- or ones in which the last and second from last generations are the same
repeating ca = ((ca !! 0) == (ca !! 1)) || ((drop 1 (ca !! 0)) == (take 399 (ca !! 1))) ||
               ((drop 1 (ca !! 1)) == (take 399 (ca !! 0))) || ((ca !! 0) == (ca !! 2)) || ((ca !! 0) == (ca !! 3)) 

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- Neighborhood-7 CA Functions

-- Builds a single generation of 7-neighborhood CA
build7Gen prev n rule
     | n == 400 = []
     | otherwise = (checkRule' rule [p (n-3), p (n-2), p (n-1), p n, p (n+1), p (n+2), p (n+3)]) : (build7Gen prev (n+1) rule)
             where p q = prev !! (mod q 400) 

-- Builds all 200 generations of 7-neighborhood CA from an initial state                                             
build7 prev gen rule
     | gen < 200 = prev : (build7 (build7Gen prev 0 rule) (gen + 1) rule)
     | otherwise = []

-- Generates and writes to file two 7-neighborhood CAs.  One with radnom initial state and the other 
-- with initial state consisting of a single live cell.
-- A third file is created which contains the CA's complete 128 binary rule, since the decimal representation
-- of the rule in the filenames is inaccurate due to overflow.
make7 rands rands' = do
  let rule = take 128 rands
  let initRand = take 400 (drop 128 rands)
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let ca = reverse (build7 initRand 0 rule)
  if repeating ca then do 
    putStrLn "Boring...trying again"
    make7 (drop 528 rands) rands'
  else do   
    let ca' = reverse (build5 init 0 rule)
    print rule
    let fileName = "Rand7Rule" ++ (show (toDec 1 (reverse rule))) ++ ".ppm"
    let fileName' = "Negh7Rule" ++ (show (toDec 1 (reverse rule))) ++ ".ppm"
--    let fileName'' = "RuleFile" ++ (show (toDec 1 (reverse rule))) ++ ".txt"
    mapDouble fileName (drawCA' rands' ca) (0,0) (400,200) (1200,600)
    mapDouble fileName' (drawCA' (drop 6 rands') ca') (0,0) (400,200) (1200,600)
--    outFile <- openFile fileName'' WriteMode
--    hPrint outFile (rule)
--    hClose outFile

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- Neighborhood 5 CA Functions

-- General rule check function used by both the larger neighborhood (5 and 7) algorithms     
checkRule' :: [Int] -> [Int] -> Int     
checkRule' rule ls = (reverse rule) !! (toDec 1 (reverse ls))     

-- Recursively build a single generation of 5-neighborhood CA
build5Gen prev n rule
     | n == 400 = []
     | otherwise = (checkRule' rule [p (n-2), p (n-1), p n, p (n+1), p (n+2)]) : (build5Gen prev (n+1) rule)
             where p q = prev !! (mod q 400) 

-- Recursively build all 200 generations of 5-neighborhood CA from an initial state                                             
build5 prev gen rule
     | gen < 200 = prev : (build5 (build5Gen prev 0 rule) (gen + 1) rule)
     | otherwise = []

-- Builds and saves to file two versions of a 5-neighborhood CA.  One with random initial state and the other
-- with initial state consisting of a single live cell.
make5 rands rands' = do
  let rule = take 32 rands
  let initRand = take 400 (drop 32 rands)
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let ca = reverse (build5 initRand 0 rule)
  if repeating ca then do 
    putStrLn "Boring...trying again"
    make5 (drop 432 rands) rands'
  else do   
    let ca' = reverse (build5 init 0 rule)
    print rule
    let fileName = "Rand5Rule" ++ (show (toDec 1 (reverse rule))) ++ ".ppm"
    let fileName' = "Negh5Rule" ++ (show (toDec 1 (reverse rule))) ++ ".ppm"
    mapDouble fileName (drawCA' rands' ca) (0,0) (400,200) (1200,600)
    mapDouble fileName' (drawCA' (drop 6 rands') ca') (0,0) (400,200) (1200,600)

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- Three state, neighborhood-3 CA functions

-- Chooses color using a list of random Doubles between 0 and 1 for a 3-state CA                 
draw3StateCA rands ca x y
     | x' == 400 || y' == 200 = black
     | otherwise = if ((ca !! y') !! x' == 0 ) then (Colour (head rands) (rands !! 1) (rands !! 2))
                   else if ((ca !! y') !! x' == 1) then (Colour (rands !! 3) (rands !! 4) (rands !! 5))
                   else (Colour (rands !! 6) (rands !! 7) (rands !! 8))
           where x' = floor x
                 y' = floor y

-- Check whether a cell of a 3-state CA is alive in the next generation                 
check3StateRule :: [Int] -> [Int] -> Int
check3StateRule rule ls = (reverse rule) !! (base3toDec 1 (reverse ls))

-- Takes an initial value of 1 and a base three number representes
-- as list (in reverse order) of 0s,1s, and 2s.
--  Returns decimal conversion
base3toDec :: Int -> [Int] -> Int
base3toDec _ [] = 0
base3toDec n (0:xs) = base3toDec (3*n) xs
base3toDec n (1:xs) = n + base3toDec (3*n) xs
base3toDec n (2:xs) = 2*n + base3toDec (3*n) xs

-- Recursively build a single generation of a 3-state 3-neighborhood CA
build3StateGen prev n rule
     | n == 400 = []
     | otherwise = (check3StateRule rule [p (n-1), p n, p (n+1)]) : (build3StateGen prev (n+1) rule)
             where p q = prev !! (mod q 400)

-- Recursively build all 200 generations of 3-state 3-neighborhood CA from an initial state                                            
build3State prev gen rule
     | gen < 200 = prev : (build3State (build3StateGen prev 0 rule) (gen + 1) rule)
     | otherwise = []

-- Build and save two versions of a 3-state 3-neighborhood CA, one from random initial position and 
-- the other from initial state consisting of a single live cell.
make3State rands rands' = do
  let rule = take 27 rands
  let initRand = take 400 (drop 27 rands)
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let ca = reverse (build3State initRand 0 rule)
  if repeating ca then do 
    putStrLn "Boring...trying again"
    make3State (drop 627 rands) rands'
  else do   
    let ca' = reverse (build3State init 0 rule)
    print rule
    let fileName = "Rand3State" ++ (show (base3toDec 1 (reverse rule))) ++ ".ppm"
    let fileName' = "ThreeStateRule" ++ (show (base3toDec 1 (reverse rule))) ++ ".ppm"
    mapDouble fileName (draw3StateCA rands' ca) (0,0) (400,200) (1200,600)
    mapDouble fileName' (draw3StateCA (drop 9 rands') ca') (0,0) (400,200) (1200,600)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- Two state neighborhood-3 CA Functions

-- Check whether cell of neighborhood-3 CA is alive in the next generation    
checkRule rule x y z 
     | (x,y,z) == (1,1,1) = rule !! 0
     | (x,y,z) == (1,1,0) = rule !! 1
     | (x,y,z) == (1,0,1) = rule !! 2
     | (x,y,z) == (1,0,0) = rule !! 3
     | (x,y,z) == (0,1,1) = rule !! 4
     | (x,y,z) == (0,1,0) = rule !! 5
     | (x,y,z) == (0,0,1) = rule !! 6
     | (x,y,z) == (0,0,0) = rule !! 7

-- Chooses color using a list of random Doubles between 0 and 1 for a 2-state CA    
drawCA' rands ca x y 
     | x' == 400 || y' == 200 = black
     | otherwise = if ((ca !! y') !! x' == 0) then (Colour (head rands) (rands !! 1) (rands !! 2))
                   else (Colour (rands !! 3) (rands !! 4) (rands !! 5))
          where x' = floor x
                y' = floor y

-- Recursively builds a single generation of 3-neighborhood CA for the previous one
buildGen' prev n rule
     | n == 400 = []     
     | otherwise = (checkRule rule (prev !! (mod (n-1) 400)) (prev !! n) (prev !! (mod (n+1) 400))) : (buildGen' prev (n+1) rule)

-- Recursively builds all 200 generations of 3-neighborhood CA from an initial state
buildCA' prev gen rule 
     | gen < 200 = prev : (buildCA' (buildGen' prev 0 rule) (gen + 1) rule) 
     | otherwise = []

-- Generate binary list representation of 2-state 3-neighborhood CA rule corresponding to integer argument.
makeRule :: Int -> [Int]
makeRule n | n == 0 = 0:[]
           | n == 1 = 1:[]
           | otherwise = (n `mod` 2) : (makeRule $ n `div` 2)

-- Generate binary list representation of 2-state 3-neighborhood CA rule corresponding to integer argument.
makeRule' :: Int -> [Int]
makeRule' n = (replicate (8 - (length ls)) 0) ++ ls
         where ls = reverse (makeRule n)

-- Takes a binary list in reverse order and start value of 1
toDec :: Int -> [Int] -> Int
toDec n [] = 0
toDec n (0:xs) = toDec (2*n) xs
toDec n (1:xs) = n + toDec (2*n) xs

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- The following three functions build a CA with population density rules.
-- The list 'survive' has a 1 at each index which corresponds to a population
-- in the neighborhood for which a cell will survive to the next generation.
-- Similarly, a 1 in the 'born' list indicates that a dead cell with that
-- index's number of neighbors will be born in the next generation.

-- Recursively build a single generation of population density CA for which the 
-- survival and birth of a cell depend on the population of its neighborhood.
buildPopGen neigh prev n survive born
     | n == 400 = []
     | otherwise = (if (prev !! n == 0 && born !! total == 1) || (prev !! n == 1 && (survive !! total == 1)) then 1
                    else 0) : (buildPopGen neigh prev (n+1) survive born)
            where total = sum $ map (\x -> prev !! (mod (n+x) 400)) ([-(div neigh 2) .. (-1)] ++ [1 .. div neigh 2])                   

-- Recursively build 200 generations of population density CA from an initial state.                               
buildPop neigh prev gen survive born
     | gen == 200 = []
     | otherwise = prev : (buildPop neigh (buildPopGen neigh prev 0 survive born) (gen+1) survive born)
   
-- Generate and save two population density CAs.  One with random initial state and one with single live starting cell.   
makePop neigh rands rands' = do
  let survive = take neigh rands
  let born = take neigh (drop neigh rands)
  let initRand = take 400 (drop (2*neigh) rands)
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let ca = reverse (buildPop neigh initRand 0 survive born) 
  --print (ca !! 0)
  --print (ca !! 1)
  if repeating ca then do 
    putStrLn "Boring...trying again"
    makePop neigh (drop (400 + (2 * neigh)) rands) rands'
  else do   
    let ca' = reverse (buildPop neigh init 0 survive born)
    putStr "Survive: "
    print survive
    putStr "Born: "
    print born
    let fileName = "RandPopNeigh" ++ (show (neigh)) ++ "S" ++ (show (toDec 1 (reverse survive))) ++ "B" ++ (show (toDec 1 (reverse born))) ++ ".ppm"
    let fileName' = "InitPopNeigh" ++ (show (neigh)) ++ "S" ++ (show (toDec 1 (reverse survive))) ++ "B" ++ (show (toDec 1 (reverse born))) ++ ".ppm"
    mapDouble fileName (drawCA' rands' ca) (0,0) (400,200) (1200,600)
    mapDouble fileName' (drawCA' (drop  6 rands') ca') (0,0) (400,200) (1200,600)

{- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- The following functions are generalized versions of the preceding ones for 3,5, and 7-neighborhood 2 and 3-state CAs.
-- They generate a CA based on # of states, and size of neighborhood
-- arguments provided by main.

-- Takes an integer n, and a seed value, and genereates an infinite list of
-- random numbers ranging from 0 to n-1.
randIntN :: Int -> Int -> [Int]
randIntN n gen = randomRs (0,(n-1)) (mkStdGen gen)

-- Helper function for drawNStateCA
getColour n rands = Colour (rands !! (3*n)) (rands !! (3*n+1)) (rands !! (3*n+2)) 

-- Generic n state CA draw function                
draw rands ca x y
     | (floor x) == 400 || (floor y) == 200 = black
     | otherwise = getColour ((ca !! (floor y)) !! (floor x)) rands
    
-- Rule check function for CA with any number of states     
check :: Int -> [Int] -> [Int] -> Int     
check states rule ls = (reverse rule) !! (baseNtoDec states 1 (reverse ls))     

-- Generic build generation function for any neighborhood size and # of states
buildGen states neigh prev n rule
     | n == 400 = []
     | otherwise = (check states rule (map (\x -> prev !! (mod (x + n) 400)) [-(div neigh 2) .. div neigh 2])) : (buildGen states neigh prev (n+1) rule) 

-- Generic build function for any neighborhood size and # of states     
build states neigh prev gen rule
     | gen == 200 = []
     | otherwise = prev : (build states neigh (buildGen states neigh prev 0 rule) (gen + 1) rule)

-- Generic to decimal conversion function
-- n = base ; total starts at 1 ; number to convert is passed in as a list in reverse order
baseNtoDec :: Int -> Int -> [Int] -> Int
baseNtoDec _ _ [] = 0
baseNtoDec n total (m:xs) = m*total + baseNtoDec n (n*total) xs

-- Takes number of states and size of environment as arguments
-- and builds CAs initialized with both random input and with a 
-- single state 1 cell
make states neigh rand1 rand2 = do
  let rule = take (floor ((fromIntegral states) ** (fromIntegral neigh))) rand1
  let initRand = take 400 (drop (floor ((fromIntegral states) ** (fromIntegral neigh))) rand1)
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let ca = reverse (build states neigh initRand 0 rule)
  if repeating ca then do 
    putStrLn "Boring...trying again"
    make states neigh (drop (400 + (floor ((fromIntegral states) ** (fromIntegral neigh)))) rand1) rand2
  else do   
    let ca' = reverse (build states neigh init 0 rule)
    print rule
    let fileName = "Rand_States" ++ (show states) ++ "Neigh" ++ (show neigh) ++ "Rule" ++ (show (baseNtoDec states 1 (reverse rule))) ++ ".ppm"
    let fileName' = "Init_States" ++ (show states) ++ "Neigh" ++ (show neigh) ++ "Rule" ++ (show (baseNtoDec states 1 (reverse rule))) ++ ".ppm"
    mapDouble fileName (draw rand2 ca) (0,0) (400,200) (1200,600) 
    mapDouble fileName' (draw (drop (3*states) rand2) ca') (0,0) (400,200) (1200,600)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-}  
-- Generate a CA with a user provided rule number
-- A desired rule number can be found by writing a list of all the transitions (e.g. (0,0,0) -> 1; (0,0,1) -> 2 ...)
-- and using the 'baseNtoDec' function to change the list from a number with base (# of states) into a decimal number.

-- Turn rule number into list of base (# of states) list
numToList' :: Int -> Int -> [Int]
numToList' base n | n < base  = n:[]
           | otherwise = (n `mod` base) : (numToList' base $ n `div` base)

-- Pad front of rule list with 0s so that it is the proper length                                           
numToList :: Int -> Int -> Int -> [Int]
numToList len base n = (replicate (len - (length ls)) 0) ++ ls
         where ls = reverse (numToList' base n)

-- Get rule number from user, rejecting rules that are out of range for the # of states and neighborhood size         
getRule states neigh = do
  num <- getInt
  if num < (floor((fromIntegral states) ** ((fromIntegral states) ** (fromIntegral neigh)))) then return num
  else do 
    putStr "Rule number not in range.  Try a smaller number: "
    getRule states neigh

-- Solicit rule number from user and make resulting CAs    
makeWithRule states neigh rand1 rand2 = do
  putStr "Enter the (decimal) number of the rule: "
  ruleNum <- getRule states neigh 
  let rule = numToList (floor((fromIntegral states) ** (fromIntegral neigh))) states ruleNum
  let init = (replicate 200 0) ++ [1] ++ (replicate 199 0)
  let initRand = take 400 (drop (floor ((fromIntegral states) ** (fromIntegral neigh))) rand1)
  let ca = reverse (build states neigh initRand 0 rule)
  let ca' = reverse (build states neigh init 0 rule)
  print rule
  let fileName = "Rule_RandStates" ++ (show states) ++ "Neigh" ++ (show neigh) ++ "Rule" ++ (show (baseNtoDec states 1 (reverse rule))) ++ ".ppm"
  let fileName' = "Rule_InitStates" ++ (show states) ++ "Neigh" ++ (show neigh) ++ "Rule" ++ (show (baseNtoDec states 1 (reverse rule))) ++ ".ppm"
  mapDouble fileName (draw rand2 ca) (0,0) (400,200) (1200,600) 
  mapDouble fileName' (draw (drop (3*states) rand2) ca') (0,0) (400,200) (1200,600)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-}  
  
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a seed value for random number generation: ";
  seed <- getInt                                                -- Seed for lists of random numbers
  let rands = randNums seed 
  let rands' = randNums' seed
  putStrLn "\n\n1D Cellular Automata Program\nOPTIONS"
  putStrLn "1. Build CA of neighborhood three"
  putStrLn "2. Find CA of neighborhood five"
  putStrLn "3. Find CA of neighborhood seven"
  putStrLn "4. Three state CA of neighborhood three"
  putStrLn "5. Choose number of states and size of neighborhood"
  putStrLn "6. Two state CA with population density rule"
  putStrLn "7. Choose CA by rule number"
  putStr "Enter the number of your selection: "
  ans <- getInt
  if ans == 1 then do                                           -- 2-state 3-neighborhood CA
    putStr "Enter rule number (0 - 255) : "
    rule <- getInt                                              -- Get rule # from user
    let fileName = "RandRule" ++ (show rule) ++ ".ppm"
    let fileName' = "Rule" ++ (show rule) ++ ".ppm"
    let ca = reverse (buildCA' (take 400 rands) 0 (makeRule' rule))
    let ca' = reverse (buildCA' ((replicate 200 0) ++ [1] ++ (replicate 199 0)) 0 (makeRule' rule))
    mapDouble fileName (drawCA' rands' ca) (0,0) (400,200) (1200,600)
    mapDouble fileName' (drawCA' (drop 6 rands') ca') (0,0) (400,200) (1200,600)
  else if ans == 2 then do
          make5 rands rands'  
  else if ans == 3 then do
          make7 rands rands'
  else if ans == 4 then do
          let rands'' = randIntN 3 seed
          make3State rands'' rands'
  else if ans == 5 then do
          putStr "\n\nEnter number of states: "
          states <- getInt
          putStr "\nEnter size of neighborhood: (Odd number from 3 - 11) "
          neigh <- getInt
          let rands'' = randIntN states seed
          make states neigh rands'' rands'
  else if ans == 6 then do
          putStr "\n\nEnter neighborhood size: (Odd number from 3 - 21) " 
          neigh <- getInt
          makePop neigh rands rands'
  else if ans == 7 then do        
          putStr "\n\nEnter number of states: "
          states <- getInt
          putStr "\nEnter size of neighborhood: (Odd number from 3 - 11) "
          neigh <- getInt
          let rands'' = randIntN states seed
          makeWithRule states neigh rands'' rands'
       else putStr ""        
  putStrLn "Exiting Cellular Automata program"  
  
