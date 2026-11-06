{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp
import Data.List (intercalate, nub)
import PySupport (PythonCode(..))

-- This is the convention we will use :

-- DivL a b  =  a \ b   (argument a on the left, result b)
-- DivR b a  =  b / a   (result b, argument a on the right)

-- enumerating all the ways of cutting a list into left/right parts
splits :: [a] -> [([a],[a])]
splits xs = [ splitAt i xs | i <- [0 .. length xs] ]

-- enumerating all ways of picking one element of a list
picks :: [a] -> [([a],a,[a])]
picks xs = [ (gl, a, gr) | (gl, a:gr) <- splits xs ]


dec_rinv :: [LTp] -> LTp -> Bool

dec_rinv gamma (DivL a b) = dec_rinv (a : gamma) b

dec_rinv gamma (DivR b a) = dec_rinv (gamma ++ [a]) b

dec_rinv gamma (Atm p)    = or [ dec_lfoc gl a gr p | (gl, a, gr) <- picks gamma ]

dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool

dec_lfoc gl (Atm q)    gr p = null gl && null gr && q == p

dec_lfoc gl (DivL a b) gr p = or [ dec_rinv gl2 a && dec_lfoc gl1 b gr p
                                 | (gl1, gl2) <- splits gl ]

dec_lfoc gl (DivR b a) gr p = or [ dec_rinv gr1 a && dec_lfoc gl b gr2 p
                                 | (gr1, gr2) <- splits gr ]

-- grammaticality

-- all the types the lexicon gives to a word
typesOf :: Lexicon sem -> String -> [LTp]
typesOf lex w = [ t | (w', t, _) <- items lex, w' == w ]

-- all ways of choosing one type for each word of a sentence
typings :: Lexicon sem -> [String] -> [[LTp]]
typings _   []     = [[]]
typings lex (w:ws) = [ t : ts | t <- typesOf lex w, ts <- typings lex ws ]

-- checking grammaticality
isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws = or [ dec_rinv gamma (stp lex) | gamma <- typings lex ws ]

-- derivations

{- Same search as dec_rinv / dec_lfoc, but every hypothesis now has a
   variable with it and we collect lambda expressions instead of outpuuting
   True/False. -}

-- a variable not already used in the context
freshFor :: [(Var,LTp)] -> Var
freshFor gamma = fresh [ V x | (x, _) <- gamma ]

der_rinv :: [(Var,LTp)] -> LTp -> [LExp]

der_rinv gamma (DivL a b) = [ L x e | e <- der_rinv ((x, a) : gamma) b ]
  where x = freshFor gamma

der_rinv gamma (DivR b a) = [ L x e | e <- der_rinv (gamma ++ [(x, a)]) b ]
  where x = freshFor gamma

der_rinv gamma (Atm p)    = [ foldl A (V x) es | (gl, (x, a), gr) <- picks gamma
                                               , es <- der_lfoc gl a gr p ]

der_lfoc :: [(Var,LTp)] -> LTp -> [(Var,LTp)] -> Atm -> [[LExp]]

der_lfoc gl (Atm q)    gr p = if null gl && null gr && q == p then [[]] else []

der_lfoc gl (DivL a b) gr p = [ e : es | (gl1, gl2) <- splits gl
                                       , e  <- der_rinv gl2 a
                                       , es <- der_lfoc gl1 b gr p ]

der_lfoc gl (DivR b a) gr p = [ e : es | (gr1, gr2) <- splits gr
                                       , e  <- der_rinv gr1 a
                                       , es <- der_lfoc gl b gr2 p ]

-- all derivations of a sentence, we use the convention that the i-th word w becomes the variable w_i
derivations :: Lexicon sem -> [String] -> [LExp]
derivations lex ws = [ e | ts <- typings lex ws, e <- der_rinv (zip names ts) (stp lex) ]
  where names = [ w ++ "_" ++ show i | (w, i) <- zip ws [1 ..] ]

-- Part 3: The goal is to keep each term together with its chosen lexical meanings.

readings :: Lexicon sem -> [String] -> [(LExp, [(Var, sem)])]
readings lex ws =
  [ (term, zip names values)
  | chosen <- entries ws
  , let (types, values) = unzip chosen
  , term <- der_rinv (zip names types) (stp lex)
  ]
  where
    names = [w ++ "_" ++ show i | (w, i) <- zip ws [1 :: Int ..]]
    entries [] = [[]]
    entries (w:rest) =
      [(tp, value) : others
      | (word, tp, value) <- items lex, word == w
      , others <- entries rest]

-- We translate the terms to Python. Lexical values are evaluated outside the generated lambdas, so their names still refer to the world database.
toPython :: [(Var, PythonCode)] -> LExp -> Either String PythonCode
toPython env term
  | length names /= length (nub names) = Left "Duplicate lexical variable"
  | otherwise = do
      body <- translate [] 0 term
      return (PCode ("(lambda _lex: " ++ body ++ ")([" ++ values ++ "])"))
  where
    names = map fst env
    positions = zip names [0 :: Int ..]
    values = intercalate ", " ["(" ++ text value ++ ")" | (_, value) <- env]

    translate :: [(Var, String)] -> Int -> LExp -> Either String String
    translate bound _ (V x) = case lookup x bound of
      Just name -> Right name
      Nothing -> case lookup x positions of
        Just i -> Right ("_lex[" ++ show i ++ "]")
        Nothing -> Left ("No meaning for " ++ x)
    translate bound depth (A f a) = do
      function <- translate bound depth f
      argument <- translate bound depth a
      return ("(" ++ function ++ ")(" ++ argument ++ ")")
    translate bound depth (L x body) = do
      let name = "_arg" ++ show depth
      result <- translate ((x, name) : bound) (depth + 1) body
      return ("(lambda " ++ name ++ ": " ++ result ++ ")")