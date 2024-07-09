module Blockify (blockify) where

{-
  Playing with blocks (or rather, creating them). Turns a decomplexified
  StructuredProgram (see Decomplexify) into a BlockProgram.
-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Types
import Compile
import StructuredProgram

blockify :: StructuredProgram -> Compile BlockProgram
blockify prog = mkCompile $ \env -> ([], Just (env, blockify' prog))

data BlockifyState = BlockifyState
  { bfs_strp :: StructuredProgram
  , bfs_blop :: BlockProgram
  , bfs_reserved :: S.Set BlockLabel
  , bfs_labelMap :: M.Map (SubroutineName, GotoLabel) BlockLabel
  }

-- Our friendly state monad.
type BF = State BlockifyState

blockify' :: StructuredProgram -> BlockProgram
blockify' strp = bfs_blop final
  where
    final = execState blockify'' initial
    initial = BlockifyState strp blop S.empty M.empty
    blop = BlockProgram
      (strp_main strp)
      M.empty
      M.empty

blockify'' :: BF ()
blockify'' = do
  strp <- getStructuredProgram
  let subNames = M.keys $ strp_subs strp
  subLabels <- mapM blockifySub subNames
  let subs = M.fromList $ zip subNames subLabels
  modifyBlockProgram $ \b -> b { blop_subs = subs }

blockifySub :: SubroutineName -> BF BlockLabel
blockifySub sub = do
  label <- reserveFreshLabel sub
  strp <- getStructuredProgram
  let stm = mustFindSubroutine strp sub
  launch sub label stm Nothing
  return label

launch :: SubroutineName -> BlockLabel -> Stm -> Maybe BlockLabel -> BF ()
launch sub blocklabel stm next = do
  block <- accum sub [] stm [] next
  addBlock blocklabel block

accum :: SubroutineName -> [AtomicStm] -> Stm -> [Stm] -> Maybe BlockLabel -> BF Block
accum sub acc stm after next =
  case stm of
    Do astm -> continue (acc ++ [astm])
    Nop -> continue acc
    Seq stm1 stm2 -> accum sub acc stm1 (stm2:after) next
    Label label -> do
      blocklabel <- reserveLabelFor sub label
      launch sub blocklabel (foldr Seq Nop after) next
      link acc blocklabel
    Goto label -> do
      ghost
      blocklabel <- reserveLabelFor sub label
      link acc blocklabel
    Return -> do
      ghost
      return $ Block acc $ NBReturn sub
    Branch condition stm1 stm2 -> do
      -- reserve the labels we need
      trueBranchLabel <- reserveFreshLabel sub
      falseBranchLabel <- reserveFreshLabel sub
      afterBranchLabel <- reserveFreshLabel sub
      -- blockify each branch
      launch sub trueBranchLabel stm1 (Just afterBranchLabel)
      launch sub falseBranchLabel stm2 (Just afterBranchLabel)
      -- blockify what comes after the branch statement
      let afterBranchStm = foldr Seq Nop after
      launch sub afterBranchLabel afterBranchStm next
      -- finish the current block and have it choose a branch
      return $ Block acc $ NBBranch condition trueBranchLabel falseBranchLabel
    Sandwich beforeCheck condition body -> do
      -- allocate the labels we need
      beforeCheckLabel <- reserveFreshLabel sub
      checkLabel <- reserveFreshLabel sub
      bodyLabel <- reserveFreshLabel sub
      afterLoopLabel <- reserveFreshLabel sub
      -- blockify the statements that run before the loop condition is checked
      launch sub beforeCheckLabel beforeCheck (Just checkLabel)
      -- create the block that branches based on the loop condition
      let checkBlock = Block [] $ NBBranch condition bodyLabel afterLoopLabel
      addBlock checkLabel checkBlock
      -- blockify the body of the loop
      launch sub bodyLabel body (Just beforeCheckLabel)
      -- blockify what comes after the loop
      let afterLoopStm = foldr Seq Nop after
      launch sub afterLoopLabel afterLoopStm next
      -- finish the current block and link it to the pre-check one
      link acc beforeCheckLabel
    _ -> error "Internal error: unexpected statement variety in Blockify.accum"
  where
    continue acc' =
      case after of
        (a:as) -> accum sub acc' a as next
        [] -> case next of
          Nothing -> return $ Block acc' $ NBHCF $
            "Dead end encountered in subroutine " ++ show sub
          Just blocklabel -> link acc' blocklabel
    link acc' blocklabel = return $ Block acc' $ NBGoto blocklabel
    -- Process seemingly-unreachable code after a "goto" or return. Not
    -- necessarily all unreachable though because it may contain labels!
    ghost =
      case after of
        [] -> return ()
        _ -> do
          ghostLabel <- reserveFreshLabel sub
          launch sub ghostLabel (foldr1 Seq after) next 

----------------------------------------
-- State manipulation
----------------------------------------


-- Create a block label for a goto-label within a subroutine, if one hasn't been
-- created for it already.
reserveLabelFor :: SubroutineName -> GotoLabel -> BF BlockLabel
reserveLabelFor sub label = do
  bfs <- get
  let labelMap = bfs_labelMap bfs
  case M.lookup (sub, label) labelMap of
    Just bl -> return bl
    Nothing -> do
      bl <- reserveFresh $ sub ++ "_" ++ label
      let labelMap' = M.insert (sub, label) bl labelMap
      modify $ \b -> b { bfs_labelMap = labelMap' }
      return bl

-- Create a fresh label for part of a subroutine.
reserveFreshLabel :: SubroutineName -> BF BlockLabel
reserveFreshLabel sub = reserveFresh sub

-- Find and reserve a fresh label based on a string.
reserveFresh :: String -> BF BlockLabel
reserveFresh s = do
  bfs <- get
  let reserved = bfs_reserved bfs
  let possibilities = [(s, i) | i <- [0..]]
  let choice = head $ filter (\bl -> bl `notElem` reserved) possibilities
  let reserved' = S.insert choice reserved
  put $ bfs { bfs_reserved = reserved' }
  return choice

getStructuredProgram :: BF StructuredProgram
getStructuredProgram = liftM bfs_strp get

modifyBlockProgram :: (BlockProgram -> BlockProgram) -> BF ()
modifyBlockProgram f = modify $ \b -> b { bfs_blop = f $ bfs_blop b }

addBlock :: BlockLabel -> Block -> BF ()
addBlock blocklabel block = do
  bfs <- get
  let blop = bfs_blop bfs
  let blocks = blop_blocks blop
  case M.lookup blocklabel blocks of
    Nothing -> do
      let blocks' = M.insert blocklabel block blocks
      let blop' = blop { blop_blocks = blocks' }
      let bfs' = bfs { bfs_blop = blop' }
      put bfs'
    Just _ -> error $ "Internal error: block already exists with label" ++
      show blocklabel
