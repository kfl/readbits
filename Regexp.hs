-- Regular expression matching, 
-- Algorithm taken from Python code at http://morepypy.blogspot.com/2010/05/efficient-and-elegant-regular.html
-- That in turn comes from Haskell code by Frank Huch and Sebastian Fischer
--
-- Author: Ken Friis Larsen <ken@friislarsen.net>
-- Copyright: (c) 2010 Ken Friis Larsen

-- Bang patterns are used to force strict evaluation of marked regexps
{-# LANGUAGE BangPatterns #-}

module Regexp where

import Data.List (foldl')


data RegExp' = Char Char
             | Epsilon
             | Alt !RegExp !RegExp
             | Rep !RegExp
             | Seq !RegExp !RegExp
             deriving (Show)
type RegExp = (RegExp', Bool, Bool)

empty (re, empty, mark) = empty
marked (re, empty, mark) = mark
    
-- Smart constructors

ch c m = (Char c, False, m)
eps m = (Epsilon, True, m)
alt re1 re2 m = (Alt re1 re2, empty re1 || empty re2, m)
rep re m = (Rep re, True, m)
re1 +++ re2 = \m -> (Seq re1 re2, empty re1 && empty re2, m)

ret re m = (re m, m)

shift (re', emp, m) c mark = 
  case re' of
    Char k -> ch c $ mark && c == k
    Epsilon -> eps False
    Alt re1 re2 -> alt left right $ marked left || marked right
                   where 
                     left = shift re1 c mark 
                     right = shift re2 c mark  
    Rep re -> rep sre $ marked sre
              where sre = shift re c (mark || m)
    Seq re1 re2 -> let left_mark = marked re1
                       sre1 = shift re1 c mark
                       sre2 = shift re2 c $ left_mark || mark && empty re1
                   in  sre1 +++ sre2 $ marked sre1 && empty re2 || marked sre2
    
match re s = null s || (marked $ foldl (\re c -> shift re c False) re1 s1)
  where
    re1 = shift re (head s) True
    s1  = tail s

match' re s = null s || (marked $ foldl (\ re c -> shift re c False) re1 s1)
  where
    re1 = shift re (head s) True
    s1  = tail s
    
chM c = ch c False
epsM = eps False
altM re1 re2 = alt re1 re2 False
repM re = rep re False
re1 *** re2 = re1 +++ re2 $ False


ex1 = chM 'a' `altM` chM 'b' `altM` chM 'c'