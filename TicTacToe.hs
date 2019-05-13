{- |
Module      :  TicTacToe.hs
Description :  Module implements a simple TicTacToe game which can be played against a computer.
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Tic-tac-toe
-}

module TicTacToe where


-- Initialize an empty board
emptyBoard = ["...", "...", "..."]

-- Return the winner
winner :: [String] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
    | a == b && b == c && a /= '.'   = a
    | d == e && e == f && d /= '.'   = d
    | g == h && h == i && g /= '.'   = g
    | a == d && d == g && a /= '.'   = a
    | b == e && e == h && b /= '.'   = b
    | c == f && f == i && c /= '.'   = c
    | a == e && e == i && a /= '.'   = a
    | c == e && e == g && c /= '.'   = c
    | '.' `elem` [a,b,c,d,e,f,g,h,i] = '?'
    | otherwise                      = '-'

-- Replace the ith element of a list
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace i a (x:xs) = x:replace (i - 1) a xs

-- Replace a character at position r, c in board
play :: Int -> Int -> Char -> [String] -> [String]
play r c a board = replace r (replace c a (board !! r)) board

-- Return the value of board with a player to play
value :: Char -> [String] -> Int
value player board
    | w == 'X'      = 1
    | w == 'O'      = -1
    | w == '-'      = 0
    | player == 'X' = maximum [value 'O' (play r c 'X' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    | otherwise     = maximum [value 'X' (play r c 'O' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    where w = winner board

-- Return, from a list of boards where X has just moved, the one with the max value
bestOf :: [[String]] -> [String]
bestOf [x] = x
bestOf (x:xs)
    | value 'O' x > value 'O' bxs = x
    | otherwise                   = bxs
    where bxs = bestOf xs

-- Return board after X's best move
bestMove :: [String] -> [String]
bestMove board = bestOf [play r c 'X' board | r <- [0..2], c <- [0..2], board !! r !! c == '.']
