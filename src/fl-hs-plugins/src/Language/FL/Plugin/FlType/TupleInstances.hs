{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Language.FL.Plugin.FlType.TupleInstances () where
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import Language.FL.Plugin.API
import Language.FL.Plugin.FlType
import Language.FL.Plugin.Foreign.Types

newTupleTail :: [Redex] -> IO Node
newTupleTail [x,y]  = flNewCons x y
newTupleTail (x:xs) = flNewCons x . flNode2Redex =<< newTupleTail xs
newTupleTail _      = error "newTupleTail called with <2 redexes"

getTupleArgs :: Int -> Node -> IO [Node]
getTupleArgs 1 n = return [n]
getTupleArgs i n = do
  x <- flGetHead n
  xs <- getTupleArgs (i-1) =<< flGetTail n
  return (x:xs)

buildTupleType :: [Ptr FlTypeExp] -> IO (Ptr FlTypeExp)
buildTupleType [t] = do
  return t
buildTupleType (t:ts) = do
  ts' <- buildTupleType ts
  flMakeTupleTy t ts'

instance (FlType a, FlType b) => FlType (a, b) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      ]
  fromNode n = do
    [a, b] <- getTupleArgs 2 n
    (,) <$> fromNode a <*> fromNode b
  overwriteRedex r (a, b) = do
    hd <- flNode2Redex <$> new a
    tl <- flNode2Redex <$> new b
    flMakeRedexCons r hd tl
  equals (a1, b1) (a2, b2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"

instance (FlType a, FlType b, FlType c) => FlType (a, b, c) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      , createFlType (Proxy @c)
      ]
  fromNode n = do
    [a, b, c] <- getTupleArgs 3 n
    (,,) <$> fromNode a <*> fromNode b <*> fromNode c
  overwriteRedex r (a, b, c) = do
    hd <- flNode2Redex <$> new a
    tl <- sequence [new b, new c]
    flMakeRedexCons r hd . flNode2Redex =<< newTupleTail (map flNode2Redex tl)
  equals (a1, b1, c1) (a2, b2, c2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    , c1 `equals` c2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"

instance (FlType a, FlType b, FlType c, FlType d) => FlType (a, b, c, d) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      , createFlType (Proxy @c)
      , createFlType (Proxy @d)
      ]
  fromNode n = do
    [a, b, c, d] <- getTupleArgs 4 n
    (,,,) <$> fromNode a <*> fromNode b <*> fromNode c <*> fromNode d
  overwriteRedex r (a, b, c, d) = do
    hd <- flNode2Redex <$> new a
    tl <- sequence [new b, new c, new d]
    flMakeRedexCons r hd . flNode2Redex =<< newTupleTail (map flNode2Redex tl)
  equals (a1, b1, c1, d1) (a2, b2, c2, d2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    , c1 `equals` c2
    , d1 `equals` d2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"

instance (FlType a, FlType b, FlType c, FlType d, FlType e) =>
    FlType (a, b, c, d, e) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      , createFlType (Proxy @c)
      , createFlType (Proxy @d)
      , createFlType (Proxy @e)
      ]
  fromNode n = do
    [a, b, c, d, e] <- getTupleArgs 5 n
    (,,,,) <$> fromNode a <*> fromNode b <*> fromNode c
           <*> fromNode d <*> fromNode e
  overwriteRedex r (a, b, c, d, e) = do
    hd <- flNode2Redex <$> new a
    tl <- sequence [new b, new c, new d, new e]
    flMakeRedexCons r hd . flNode2Redex =<< newTupleTail (map flNode2Redex tl)
  equals (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    , c1 `equals` c2
    , d1 `equals` d2
    , e1 `equals` e2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"

instance (FlType a, FlType b, FlType c, FlType d, FlType e, FlType f) =>
    FlType (a, b, c, d, e, f) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      , createFlType (Proxy @c)
      , createFlType (Proxy @d)
      , createFlType (Proxy @e)
      , createFlType (Proxy @f)
      ]
  fromNode n = do
    [a, b, c, d, e, f] <- getTupleArgs 6 n
    (,,,,,) <$> fromNode a <*> fromNode b <*> fromNode c
            <*> fromNode d <*> fromNode e <*> fromNode f
  overwriteRedex r (a, b, c, d, e, f) = do
    hd <- flNode2Redex <$> new a
    tl <- sequence [new b, new c, new d, new e, new f]
    flMakeRedexCons r hd . flNode2Redex =<< newTupleTail (map flNode2Redex tl)
  equals (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    , c1 `equals` c2
    , d1 `equals` d2
    , e1 `equals` e2
    , f1 `equals` f2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"

instance (FlType a, FlType b, FlType c, FlType d, FlType e, FlType f, FlType g) =>
    FlType (a, b, c, d, e, f, g) where
  isBaseType _ = True
  createFlType _ = do
    buildTupleType =<< sequence
      [ createFlType (Proxy @a)
      , createFlType (Proxy @b)
      , createFlType (Proxy @c)
      , createFlType (Proxy @d)
      , createFlType (Proxy @e)
      , createFlType (Proxy @f)
      , createFlType (Proxy @g)
      ]
  fromNode n = do
    [a, b, c, d, e, f, g] <- getTupleArgs 7 n
    (,,,,,,) <$> fromNode a <*> fromNode b <*> fromNode c
             <*> fromNode d <*> fromNode e <*> fromNode f
             <*> fromNode g
  overwriteRedex r (a, b, c, d, e, f, g) = do
    hd <- flNode2Redex <$> new a
    tl <- sequence [new b,new c,new d,new e,new f,new g]
    flMakeRedexCons r hd . flNode2Redex =<< newTupleTail (map flNode2Redex tl)
  equals (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2) = and
    [ a1 `equals` a2
    , b1 `equals` b2
    , c1 `equals` c2
    , d1 `equals` d2
    , e1 `equals` e2
    , f1 `equals` f2
    , g1 `equals` g2
    ]
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"
