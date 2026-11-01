{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

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
