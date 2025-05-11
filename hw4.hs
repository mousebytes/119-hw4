-- Alfonso Morales : ID 301975025
import Data.List
import Data.Char
import Debug.Trace

-- CFG Def
type CFG = (Char,Char->Char->[String])

accept ::CFG-> [Char]->Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
    lq a [] = []
    lq a (x:xs) | isUpper x = map (++ xs) $ d x a -- nonterm
                | otherwise = if a==x then [xs] else [] --terminal

{-
ex 1: balanced parentheses (not including empty str)
original: S --> () | (S) | SS

in TNF: S--> () | ()S | (S) | (S)S
-}

bp :: CFG
bp = ('S',d) where
    d 'S' '(' = [")",")S","S)","S)S"]
    d 'S' ')' = []

{-
ex 2: {a^1 b^j c^{i+j} | i >=0, j>0}
original: S--> aSc | T
          T--> bTc | bc

in TNF:   S--> aSc | bTc | bc
          T--> bTc | bc
-}

p1 = ('S',d) where
    d 'S' 'a' = ["Sc"] ; d 'S' 'b' = ["Tc","c"] ; d 'S' 'c' = []
    d 'T' 'a' = [] ; d 'T' 'b' = ["Tc","c"] ; d 'T' 'c' = []


-- PART 1

testMyString::String -> Bool
testMyString xs = count 'a' xs == count 'b' xs
    where count c = length . filter (== c)

sameNumABs :: CFG
sameNumABs = ('S', d) where
    d 'S' 'a' = ["Sb","b","SbT","bT"] ; d 'S' 'b' = ["Sa","a","SaT","aT"]
    d 'T' 'a' = ["Sb","b","SbT","bT"] ; d 'T' 'b' = ["Sa","a","SaT","aT"]

--stole from FLang -> replaced sigma variable from FLang with "ab" to constrain strs :)
strings :: Int -> [String]
strings n = concat [strs i | i <- [1..n]] where -- did a 1 to ignore epsilon
  strs 0 = [""]
  strs n = [a:xs | a <- "ab", xs <- strs (n-1)]

-- checking acceptance of str w in that it complies with what the grammar expects
-- w is some str sigma* = {a,b} of length >= 10
test1 = and [testMyString w == accept sameNumABs w | w <- strings 10]

findFailures =[ w | w <- strings 10, testMyString w /= accept sameNumABs w ]

{-
ghci> findFailures
[]
ghci> test1
True
-}



----- PART 2 -----

-- tape head dir (left,right)
data Dir = L | R

-- res of transition (a is state type)
data Res a = Reject | Accept | Step a Char Dir

-- turing machine M = (Q,s,d)
type TM a = ([a],a,a->Char->Res a)

-- xs in [a^i b^i c^i | i in [0..]]
abc :: String -> Bool
abc xs = let n = length $ takeWhile (=='a') xs
             block x = replicate n x
         in xs == concatMap block "abc"

-- all strings on alphabet {a,b,c} of length n or less
stringsABC :: Int ->[String]
stringsABC n = concat [strs i | i <-[0..n]] where
    strs 0 = [""]
    strs n = [a:xs | a <- "abc",xs <-strs(n-1)]

-- TM acceptance
acceptTM :: TM a -> String -> Bool
acceptTM (qs,s,d) w = run [] s w where
    -- run as q bs: q is current state as in reversed left side of tape
    -- bs is right side of tape
    run as q [] = run as q " " -- reached right end of type: add blank
    run as q (b:bs) = case d q b of
        Reject -> False
        Accept -> True
        Step q' a' L -> case as of
            []->False -- tried to move off left end of tape

            x:xs -> run xs q' (x:a':bs)
        Step q' a' R -> run (a':as) q' bs

tmabcs :: TM Int
tmabcs = ([0..5],0,d) where
    d 0 'b' = Reject ; d 0 'c' = Reject ; d 0 'a' = Step 1 'X' R ; d 0 ' ' = Accept ; d 0 'X' = Step 0 'X' R
    d 1 'b' = Step 2 'Y' R ; d 1 'a' = Step 1 'a' R ; d 1 'Y' = Step 1 'Y' R ; d 1 'c' = Reject ; d 1 'Z' = Reject ; d 1 ' ' = Reject
    d 2 'c' = Step 3 'Z' R ; d 2 'b' = Step 2 'b' R ; d 2 'Z' = Step 2 'Z' R ; d 2 'a' = Reject ; d 2 ' ' = Reject
    d 3 'c' = Step 3 'c' L; d 3 'b' = Step 3 'b' L; d 3 'a' = Step 3 'a' L; d 3 'Z' = Step 3 'Z' L ; d 3 'Y' = Step 3 'Y' L ; d 3 'X' = Step 4 'X' R ; d 3 ' ' = Step 5 ' ' L

    d 4 'a' =  Step 0 'a' L ; d 4 'Y' = Reject ; d 4 'Z' = Reject ; 
    d 5 'Z' = Step 5 'Z' L ; d 5 'Y' = Step 5 'Y' L ; d 5 'a' = Reject ; d 5 'b' = Reject ; d 5 'c' = Reject 
    d _ _ = Accept

-- TM acceptance
acceptTrace :: Show a => TM a -> String -> Bool   -- Show a needed to output state
acceptTrace (qs, s, d) w = run [] s w where
  -- run as q bs: q is current state, as in reversed left side of tape,
  -- bs is right side of tape
  run as q xs | trace ('|' : reverse as ++ "[" ++ show q ++ "]" ++ xs) False = undefined
  run as q [] = run as q " "  -- reached right end of tape: add blank
  run as q (b:bs) = case d q b of
                      Reject -> False
                      Accept -> True
                      Step q' a' L -> case as of
                                        [] -> False  -- tried to move off left end of tape
                                        x:xs -> run xs q' (x:a':bs)
                      Step q' a' R -> run (a':as) q' bs

test2 = and [ abc w == acceptTM tmabcs w |  w <- stringsABC 15]
--test1 = and [testMyString w == accept sameNumABs w | w <- strings 10]
findFailures2 =[ w | w <- stringsABC 15, abc w /= acceptTM tmabcs w ]

{-
ghci> test2 
True
ghci> findFailures2
[]
-}