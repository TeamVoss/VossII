module StructuredProgram where

{-
  Helpful stuff for the sequential program representation.
-}

import qualified Data.Map as M
import Types


----------------------------------------
-- Lookups
----------------------------------------

findSubroutine :: StructuredProgram -> SubroutineName -> Maybe Stm
findSubroutine prog name = M.lookup name $ strp_subs prog

mustFindSubroutine :: StructuredProgram -> SubroutineName -> Stm
mustFindSubroutine prog name =
  case findSubroutine prog name of
    Just sub -> sub
    _ -> error $ "Could not find subroutine: " ++ show name


----------------------------------------
-- Statement helpers
----------------------------------------

-- What statements are direct children of a statement?
subStms :: Stm -> [Stm]
subStms stm =
  case stm of
    Seq s1 s2 -> [s1,s2]
    Branch _ s1 s2 -> [s1,s2]
    While _ s -> [s]
    For s1 _ s2 s3 -> [s1,s2,s3]
    Sandwich s1 _ s2 -> [s1,s2]
    _ -> []
