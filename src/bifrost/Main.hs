import System.Environment (getArgs)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO (Handle, IOMode(WriteMode), hPutStrLn, hPrint, withFile)
import Data.List (isSuffixOf, replicate, findIndex)
import Control.Monad (forM, forM_, when)
import Data.Char (isSpace)

import Types
import Environment
import Message
import Compile
import StrUtil (indent)

-- BNFC
import qualified Lang.AbsLang as Lang
import qualified Lang.ParLang as ParLang
import qualified Lang.ErrM as ErrLang

-- Stages
import Preprocessor
import ReadAST
import RefCheck
import FixSharedActions
import Typecheck
import TypecheckStructuredProgram
import ControlFlowCheck
import Decomplexify
import Blockify
import Plan
import Liveness
import Unshare
import Hardware
import FLOut

main :: IO ()
main = do
  args <- getArgs
  statuses <- forM args $ \fname -> do
    if any (`isSuffixOf` fname) source_extensions
      then processFile fname (fname ++ ".fl") (fname ++ ".log")
      else die $ "To avoid unhappy accidents, only files with extensions " ++ unwords source_extensions ++ " are allowed."
  let count = length statuses
  let sucs = length $ filter id statuses
  let remark = if sucs == count then "Yay!" else "ALERT! Only"
  putStrLn ""
  putStrLn $ replicate 80 '-'
  putStrLn $ remark ++ " " ++ show sucs ++ "/" ++ show count ++
    " program(s) compiled successfully."
  if sucs == count
      then exitSuccess
      else exitFailure

source_extensions :: [String]
source_extensions = [".bfst"]

printIntermediate :: Bool
printIntermediate = True

processFile :: FilePath -> FilePath -> FilePath -> IO Bool
processFile fileIn fileOut fileLog = withFile fileLog WriteMode $ \log -> do
  stuff <- loadFile fileIn
  let s = unlines $ map fst stuff
  hPutStrLn log "\n-------- original source code (with #includes expanded) --------\n"
  hPutStrLn log s
  hPutStrLn log "\n"
  case ParLang.pProgram (ParLang.myLexer s) of
    ErrLang.Ok lprog -> do
      let pr = printIntermediate
      let no = False
      (mres, msgs) <-
        begin lprog emptyEnvironment $
        stage pr log "readAST" readAST $
        stage no log "refcheck" refCheck $
        stage no log "fixSharedActions" fixSharedActions $
        stage no log "typecheck" typecheckStructuredProgram $
        stage no log "controlflowcheck" controlFlowCheck $
        stage pr log "decomplexify" decomplexify $
        stage pr log "blockify" blockify $
        stage pr log "plan" plan $
        stage pr log "liveness" liveness $
        stage pr log "unshare" unshare $
        stage no log "hardware" hardware $
        stage no log "fl" genFL $
        done
      --putStrLn $ replicate 60 '-'
      putStrLn $ "Compilation of " ++ fileIn ++ " " ++ maybe "failed" (\_ -> "succeeded!") mres
      when (not $ null msgs) $ do
          --putStrLn "Message log:"
          mapM_ print msgs
          --putStrLn ""
      case mres of
        Nothing -> return False
        Just s -> do
          writeFile fileOut s
          -- putStrLn $ "Compiled program written to " ++ fileOut
          -- putStrLn ":)"
          return True
    ErrLang.Bad err -> do
      let errStr = unlines $ formatError stuff s err
      putStrLn errStr
      hPutStrLn log errStr
      return False

formatError :: [SourcedLine] -> String -> String -> [String]
formatError sourceLines source err =
  let printables = [err, "", provenanceMsg, "", contextMsg]
  in printables
  where
    provenanceMsg :: String
    provenanceMsg = maybe "(could not find line provenance)" id provenance
    provenance :: Maybe String
    provenance = do
      idx <- lineIndex
      foo <- pick idx sourceLines
      let (line, LineSource path num) = foo
      return $ "Really line " ++ show num ++ " from " ++ path ++ ", which after preprocessing looks kinda like:\n" ++
        bump (trim line)
    contextMsg :: String
    contextMsg = maybe "(could not find context)" id context
    context :: Maybe String
    context = do
      idx <- lineIndex
      let relevant = take 5 $ drop (idx - 2) sourceLines
      let texts = filter (not . null) $ map (trim . fst) relevant
      return $ unlines $ "Context:" : map bump texts
    lineIndex :: Maybe Int
    lineIndex = do
      let ws = words err
      i <- findIndex (== "line") ws
      numStr <- pick (i + 1) ws
      num <- parseInt numStr  -- line number starting at 1
      let idx = num - 1       -- line number starting at 0
      return idx
    pick :: Int -> [a] -> Maybe a
    pick i xs = if i >= 0 && i <= length xs
      then return (xs !! i)
      else Nothing
    parseInt :: String -> Maybe Int
    parseInt s = case reads s of
      [(i,_)] -> return i
      _ -> Nothing
    trim = foo . foo
    foo = reverse . dropWhile isSpace
    bump = indent 2
      
      
type Stage r a = a -> Environment -> IO (Maybe r, [Message])

begin :: a -> Environment -> (Stage r a) -> IO (Maybe r, [Message])
begin prog env stg = stg prog env

stage :: Show b => Bool -> Handle -> String -> (a -> Compile b) -> Stage r b -> Stage r a
stage printProg logh name c next = \prog env -> do
  let (msgs, res) = runCompile (c prog) env
  case res of
    Just (env', prog') -> do
      printStatus "succeeded"
      when printProg $ do
        logLn ""
        logLn (show prog')
        logLn ""
      printMessages msgs
      (res', msgs') <- next prog' env'
      return (res', msgs ++ msgs')
    Nothing -> do
      printStatus "failed"
      printMessages msgs
      return (Nothing, msgs)
  where
    printStatus s = logLn $
      "-------- Compilation stage " ++ show name ++ " " ++ s ++ "! --------"
    printMessages msgs =
      if not $ null msgs
        then do
          logLn "\nMessages:"
          mapM_ (logLn . show) msgs
          logLn ""
        else return ()
    logLn = hPutStrLn logh

done :: Stage r r
done = \res _ -> return $ (Just res, [])
