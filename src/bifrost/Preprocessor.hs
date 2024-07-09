module Preprocessor (loadFile, LineSource (LineSource), SourcedLine) where

import Data.List
import Data.Char (isSpace)
import System.FilePath

data LineSource = LineSource FilePath Integer
type SourcedLine = (String, LineSource)

loadFile :: FilePath -> IO [SourcedLine]
loadFile file = do
  s <- readFile file
  lineses <- mapM (expandLine file) $ zip [1..] $ lines s
  return $ concat lineses

expandLine :: FilePath -> (Integer, String) -> IO [SourcedLine]
expandLine file (num, line) =
  if "#" `isPrefixOf` line
    then doCmd $ splitWord $ tail line
    else return [(line, LineSource file num)]
  where
    doCmd (cmd, rest) =
      case cmd of
        "include" ->
          case reads rest :: [(String, String)] of
            [(path, s)] -> do
              let relpath = takeDirectory file </> path
              let fullpath = if "/" `isPrefixOf` path then path else relpath
              foo <- loadFile fullpath
              return $ foo ++ [(s, LineSource file num)]
            _ -> bad $ "bad invocation of #include (did you forget quotes around the file name, or forget the name entirely?)"
        _ -> bad $ "unintelligible preprocessor directive"
    splitWord s =
      case findIndex isSpace s of
        Just i -> splitAt i s
        Nothing -> (s, "")
    bad s = error $ file ++ " line " ++ show num ++ ": " ++ s ++ "\n  " ++ line
