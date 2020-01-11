{-# OPTIONS_GHC -w #-}

module Test.TestExts (
  tstExts
) where

import Prelude
import GHC.Exts (Constraint)


newtype C  = C { a :: Int }

foo :: C -> Int -> Int
foo (C {a}) i = a


data Type = App String [Type]

pattern ArrowPattern t1 t2 = App "->" [t1, t2]

bar :: Type -> (Type, Type)
bar (ArrowPattern t1 t2) = (t1, t2)


pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)


-- | PolyKinds allows to use kinds as types
--data SomeGadt (a :: k) where
--  MkEmpty :: SomeGadt a
--  MkK :: SomeGadt a -> k -> SomeGadt Int
--
--tstPolyKinds = MkK (MkEmpty @'True) True
--
--data TypeInType k (a :: k) = TypeInType k
--
--tstTypeInType :: TypeInType Bool 'True
--tstTypeInType = TypeInType True


type family MkConst a b :: Constraint where
  MkConst Int b = Num b
  MkConst Bool b = Enum b

tstConstr :: MkConst Bool a => a -> Int
tstConstr = fromEnum

type TstFlexSyn a = forall b . Show b => a -> b -> (a, String)

type Stringy a = (Read a, Show a)


tstImplicit :: (?s :: String) => String
tstImplicit = ?s

tstExecImplicit :: String
tstExecImplicit = let ?s = "kekpuk" in tstImplicit


-- | partial signature; will show deduction as warning
--tstPertialSig :: [a] -> a -> _r
--tstPertialSig [] a     = a
--tstPertialSig (a:as) _ = a

-- | DatatypeContexts
-- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=datatypecontexts#extension-DatatypeContexts
--data Show a => TstConst a = TstConst a


tstExts :: IO ()
tstExts = do
  putStrLn "TestExts"
  print 0
