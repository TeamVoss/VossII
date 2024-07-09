module StrUtil where

import Data.List (findIndex, intersperse, isPrefixOf, isSuffixOf, splitAt, tails)

bracket :: String -> String
bracket s = "[" ++ s ++ "]"

hasParens :: String -> Bool
hasParens s = "(" `isPrefixOf` s && ")" `isSuffixOf` s

indent :: Int -> String -> String
indent n = (++) (replicate n ' ')

indentLines :: Int -> String -> String
indentLines n = mapLines (indent n)

joinWith :: [a] -> [[a]] -> [a]
joinWith delim xs = concat $ intersperse delim xs

splitBy :: Eq a => [a] -> [a] -> [[a]]
splitBy delim xs =
  case findIndex (delim `isPrefixOf`) (tails xs) of
    Nothing -> [xs]
    Just i ->
      let (ys, zs) = splitAt i xs
      in ys:(splitBy delim (drop (length delim) zs))

mapLines :: (String -> String) -> String -> String
mapLines f x =
  let y = unlines $ map f $ lines x
  in if "\n" `isSuffixOf` x
    then y
    else reverse $ tail $ reverse y

paren :: String -> String
paren s = "(" ++ s ++ ")"

parenIfNeeded :: String -> String
parenIfNeeded s = if hasParens s then s else paren s

semi :: String -> String
semi s = s ++ ";"

unsemi :: String -> String
unsemi s =
  if ";" `isSuffixOf` s
    then reverse $ tail $ reverse s
    else s
