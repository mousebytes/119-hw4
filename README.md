assignment 4 from csci 119 fresno state

Part 1: Parsing Terminal Normal Form grammars.

Recall that a context free grammar G = (N,S,R) over an alphabet Σ is in terminal normal form (TNF) if every rule in R  has the form X ⟶ aγ, where a ∈ Σ and γ ∈ (N∪Σ)∗ . In short, the RHS of every rule of G starts with a terminal symbol. Given a grammar in TNF, we can construct a kind of nondeterministic machine that accepts the same language as G , except that it might have an infinite number of states:

the states of the machine are the elements of (N∪Σ)∗, i.e., strings of nonterminals and terminals
the start state is S (a string of length 1)
the final state (there is only one) is the empty string
the transition function δ is defined, for any γ ∈ (N∪Σ)∗, a,b ∈ Σ , and X ∈ N, by the rules
δ(ε, a) = {}
δ(Xγ, a) = {βγ ∣ X ⟶ aβ ∈ R}
δ(bγ, a) = if  a = b then {γ} else {} .
The state of the machine represents the string of terminals and nonterminals that we still have to parse, and the transition function explains what happens to the state as we read a single character. This machine is nondeterministic because its transition function returns a set of states -- maybe empty, maybe a singleton, maybe more than one. We can run this machine the same way we do in the subset construction: keep track of a set of states, transition on each state in the set, and accept if the final state is among the states reached by the end. Here is a complete Haskell implementation, along with two examples:

import Data.List (foldl')
import Data.Char (isUpper)

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal

-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []

-- Example 2: {a^i b^j c^{i+j} | i >= 0, j > 0}
-- original: S --> aSc | T
--           T --> bTc | bc
-- in TNF:   S --> aSc | bTc | bc
--           T --> bTc | bc
pl = ('S', d) where
  d 'S' 'a' = ["Sc"]      ;  d 'S' 'b' = ["Tc","c"]  ;  d 'S' 'c' = []
  d 'T' 'a' = []          ;  d 'T' 'b' = ["Tc","c"]  ;  d 'T' 'c' = []
In this code, nonterminals are represented by uppercase letters, and all other characters are treated as terminals.

Part 1 of the assignment concerns the grammar S ⟶ aSb ∣ bSa ∣ SS ∣ ε.

What set of strings does this grammar generate? Write an independent Haskell function of type String -> Bool to check whether a string meets this condition.
Put this grammar in TNF and encode it as a Haskell expression of type CFG.
Test acceptance of this grammar on all strings of length 10 or less, using the parsing code above, against your Haskell function from #1 .
 

Part 2: Turing Machines. Here is some code for running Turing Machine programs

-- Tape head direction (left, right)
data Dir = L | R

-- Result of a transition (a is state type)
data Res a = Reject | Accept | Step a Char Dir

-- Turing machine M = (Q, s, d)
type TM a = ([a], a, a -> Char -> Res a)

-- xs in [a^i b^i c^i | i in [0..]]
abc :: String -> Bool
abc xs = let n = length $ takeWhile (=='a') xs
             block x = replicate n x
         in xs == concatMap block "abc"

-- All strings on alphabet {a,b,c} of length n or less
strings :: Int -> [String]
strings n = concat [strs i | i <- [0..n]] where
  strs 0 = [""]
  strs n = [a:xs | a <- "abc", xs <- strs (n-1)]

-- TM acceptance
accept :: TM a -> String -> Bool
accept (qs, s, d) w = run [] s w where
  -- run as q bs: q is current state, as in reversed left side of tape,
  -- bs is right side of tape
  run as q [] = run as q " "  -- reached right end of tape: add blank
  run as q (b:bs) = case d q b of
                      Reject -> False
                      Accept -> True
                      Step q' a' L -> case as of
                                        [] -> False  -- tried to move off left end of tape
                                        x:xs -> run xs q' (x:a':bs)
                      Step q' a' R -> run (a':as) q' bs
For Part 2 of the assignment, create a Turing Machine (for example an expression of type TM Int) that accepts the same language as the function abc does. Test it against against this function for all strings of length 15 or less.

Turn in both parts of your assignment as a single Haskell file, hw4.hs.
