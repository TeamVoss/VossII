module BlockProgram where

import qualified Data.Map as M
import Types

findBlock :: BlockProgram -> BlockLabel -> Maybe Block
findBlock prog label = M.lookup label (blop_blocks prog)

findSubroutineBlockLabel :: BlockProgram -> SubroutineName -> Maybe BlockLabel
findSubroutineBlockLabel prog name = M.lookup name (blop_subs prog)

mustFindBlock :: BlockProgram -> BlockLabel -> Block
mustFindBlock prog label =
  case findBlock prog label of
    Just block -> block
    Nothing -> error $ "Could not find block with label " ++ show label

mustFindSubroutineBlockLabel :: BlockProgram -> SubroutineName -> BlockLabel
mustFindSubroutineBlockLabel prog name =
  case findSubroutineBlockLabel prog name of
    Just label -> label
    Nothing -> error $ "Could not find block label for subroutine " ++ show name

    
