{-
  Here we generate the grammar for expressions (which is also used for
  left-hand-sides of assignment statements).

  Each variety of expression is viewed as being bound with a certain tightness
  ("precedence"); this is how associativity and ambiguity are resolved. This
  results in rules such as the following one for addition:

      expression that's as tightly bound as a sum (or more)
                    v
    Plus. Exp4 ::= Exp4 "+" Exp5 ;
                             ^
                expression that's strictly more tightly bound than a sum

  Unfortunately managing these level numbers by hand is tiresome, hence this
  program. The idea is that we designate both the precedence level of each
  expression variety (by arranging them in bins), and also how each variety's
  precedence level relates to that of each of its sub-expressions (for example:
  Plus has two sub-expressions, the first being of the same precedence level and
  the second being of the next (i.e. more tightly bound) level).

  Combined with some formatting code for each variety (as some aren't simply
  binary operators) this gives us enough information to generate the grammar.
-}


import Prelude hiding (exp)
import Data.List (sort)

-- Yeehaw
main :: IO ()
main = putStrLn grammar


-- 1. Define everything

-- Expression varieties
data E
  = Var
  | IntLit
  | Blob
  | IfThenElse
  | App
  | Field
  | TupleZero
  | TupleTwoPlus
  | Proj
  | TypeAnn
  | Call
  -- | ActionCall
  -- | SubCall
  | Ignore -- lhs only, "_"
  | Not
  | Or
  | And
  | Xor
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | ShiftL
  | ShiftR
  | ArithShiftR
  | Str
  | Dict
  deriving (Eq, Ord, Show)

everything :: [E]
everything =
  [ Var
  , IntLit
  , Blob
  , IfThenElse
  , App
  , Field
  , TupleZero
  , TupleTwoPlus
  , Proj
  , TypeAnn
  , Call
  -- , ActionCall
  -- , SubCall
  , Ignore
  , Not
  , Or
  , And
  , Xor
  , Eq
  , Neq
  , Lt
  , Gt
  , Lte
  , Gte
  , Plus
  , Minus
  , Times
  , Div
  , Mod
  , ShiftL
  , ShiftR
  , ArithShiftR
  , Str
  , Dict
  ]


-- Least to most tight level of resulting expression.
precedence :: [(Integer, [E])]
precedence = zip [0..] $
  [ [IfThenElse]
  , [Or]
  , [And, Xor]
  , [Eq, Neq, Lt, Gt, Lte, Gte]
  , [Plus, Minus]
  , [Times, Div, Mod]
  , [ShiftL, ShiftR, ArithShiftR]
  , [Not]
  , [Call]
  -- , [SubCall, ActionCall]
  , [App]
  , [TypeAnn]
  , [Blob]
  , [Field, Proj, Var, IntLit, Ignore, TupleTwoPlus, TupleZero, Str, Dict]
  ]


data Rec = One RecLevel | List RecLevel  -- note: we don't actually use the one/list distinction at the moment
data RecLevel = Top | Same | Tighter | LevelOf E

-- What kind of sub-expressions each form has.
recurrences :: E -> [Rec]
recurrences e =
  case e of
    Var -> none
    IntLit -> none
    Blob -> none
    IfThenElse -> [One Same, One Same, One Same]
    App -> binAssocLeft
    Field -> unary
    TupleZero -> none
    TupleTwoPlus -> [One Top, List Top]
    Proj -> unary
    TypeAnn -> unary
    Call -> none  -- we do it by proxy
    --ActionCall -> none  -- we do it by proxy
    --SubCall -> none  -- we do it by proxy
    Ignore -> none
    Not -> unary
    Or -> binAssocLeft
    And -> binAssocLeft
    Xor -> binAssocLeft
    Eq -> binNoAssoc
    Neq -> binNoAssoc
    Lt -> binNoAssoc
    Gt -> binNoAssoc
    Lte -> binNoAssoc
    Gte -> binNoAssoc
    Plus -> binAssocLeft
    Minus -> binAssocLeft
    Times -> binAssocLeft
    Div -> binAssocLeft
    Mod -> binAssocLeft
    ShiftL -> binAssocLeft
    ShiftR -> binAssocLeft
    ArithShiftR -> binAssocLeft
    Str -> none
    Dict -> none
  where
    none = []
    unary = [One Same]  -- assumes precedence levels sort out left vs right ambiguity
    binAssocLeft = [One Same, One Tighter]
    binAssocRight = [One Tighter, One Same]
    binNoAssoc = [One Tighter, One Tighter]

-- Returns rule name, and rule rhs words.
makeRule :: E -> Integer -> [Integer] -> (String, [String])
makeRule e myLevel subLevels =
  case e of
    Var -> custom $ \[] -> ["VarName"]
    IntLit -> custom $ \[] -> ["Integer"]
    Blob -> custom $ \[] -> [quote "fl", "String"]
    IfThenElse -> custom $ \[l1, l2, l3] ->
      [quote "if", exp l1,
       quote "then", exp l2,
       quote "else", exp l3]
    App -> custom $ \[l1, l2] -> [exp l1, exp l2]
    Field -> custom $ \[l] -> [exp l, quote "-->", "FieldName"]
    TupleZero -> custom $ \[] -> [quote "()"]
    TupleTwoPlus -> custom $ \[l1, l2] ->
      [quote "(", exp l1, quote ",", explist l2, quote ")"]
    Proj -> custom $ \[l] -> [exp l, quote ".", "Integer"]
    TypeAnn -> custom $ \[l] -> [exp l, quote "::", "Type"]
    Call -> custom $ \[] -> ["Call"]
    -- ActionCall -> custom $ \[] -> ["ActionCall"]
    -- SubCall -> custom $ \[] -> ["SubCall"]
    Ignore -> custom $ \[] -> [quote "_"]
    Not -> custom $ \[l] -> [quote "~", exp l]
    Or -> bin "|"
    And -> bin "&"
    Xor -> bin "^"
    Eq -> bin "=="
    Neq -> bin "!="
    Lt -> bin "<"
    Gt -> bin ">"
    Lte -> bin "<="
    Gte -> bin ">="
    Plus -> bin "+"
    Minus -> bin "-"
    Times -> bin "*"
    Div -> bin "/"
    Mod -> bin "%"
    ShiftL -> bin "<<"
    ShiftR -> bin ">>"
    ArithShiftR -> bin "|>>"
    Str -> custom $ \[] -> ["String"]
    Dict -> custom $ \[] -> ["Dict"]
  where
    custom f = build $ f subLevels
    bin sym = build $ case subLevels of
        [left, right] -> [exp left, quote sym, exp right]
    build rhs = (show e, rhs)
    explist l = "[" ++ exp l ++ "]"
    paren ws = [quote "("] ++ ws ++ [quote ")"]


-- 2. Generate the grammar

exp :: Integer -> String
exp i = "Exp" ++
  case i of
    0 -> ""
    _ -> show i

quote :: String -> String
quote s = "\"" ++ s ++ "\""


level :: E -> Integer
level e =
  case filter ((e `elem`) . snd) precedence of
    [(l, _)] -> l
    _ -> error $ "Bad precedence for " ++ show e


sorted :: [(Integer, E)]
sorted = sort $  map (\e -> (level e, e)) everything

maxLevel :: Integer
maxLevel = maximum $ map fst sorted  -- max level (tightest) *produced*

numCoercions :: Integer
numCoercions = maxLevel + 1

grammar :: String
grammar =
  unlines $ (map printRule sorted) ++ [coercions, extra]
  where
    coercions =
      unwords ["coercions", exp 0, show numCoercions, ";"]
    extra =
      unwords ["separator nonempty", exp 0, quote ",", ";"]
    printRule (l, e) =
      let subls = map (unrelativize l) $ recurrences e
          (name, rhswords) = makeRule e l subls
      in unwords $ ["E" ++ name ++ ".", exp l, "::="] ++ rhswords ++ [";"]
    unrelativize l r = case r of
      One rl -> unrelativize' l rl
      List rl -> unrelativize' l rl
    unrelativize' l rl = case rl of
      Top -> 0
      Same -> l
      Tighter -> l + 1
      LevelOf e -> level e
