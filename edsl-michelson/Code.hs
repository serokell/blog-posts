{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Typeable
import Data.Vinyl

data T =
   TInt
 | TNat
 | TList T
 | TPair T T

data UVal =
    UInt  Int
  | UList [UVal]
  | UPair UVal UVal

data UInstr =
    UNOP
  | UDROP
  | UDUP
  | USWAP
  | UPUSH T UVal
  | UPAIR
  | UCAR
  | UCDR
  | UADD
  | UCONS
  | UNIL T
  | UIF_CONS [UInstr] [UInstr]

data Val t where
  VInt :: Int -> Val 'TInt
  VNat :: Word -> Val 'TNat
  VList :: [Val t] -> Val ('TList t)
  VPair :: Val p -> Val q -> Val ('TPair p q)

data Instr (inp :: [T]) (out :: [T]) where
  Seq :: Instr a b -> Instr b c -> Instr a c
  Nop :: Instr s s

  DROP :: Instr (a ': s) s
  DUP  :: Instr (a ': s) (a ': a ': s)
  SWAP :: Instr (a ': b ': s) (b ': a ': s)
  PUSH :: Val t -> Instr s (t ': s)

  PAIR :: Instr (a ': b ': s) ('TPair a b ': s)
  CAR :: Instr ('TPair a b ': s) (a ': s)
  CDR :: Instr ('TPair a b ': s) (b ': s)

  NIL  :: Instr s ('TList t ': s)
  CONS :: Instr (t ': 'TList t ': s) ('TList t ': s)

  ADDii :: Instr ('TInt ': 'TInt ': s) ('TInt ': s)
  ADDnn :: Instr ('TNat ': 'TNat ': s) ('TNat ': s)
  ADDin :: Instr ('TInt ': 'TNat ': s) ('TInt ': s)
  ADDni :: Instr ('TNat ': 'TInt ': s) ('TInt ': s)

  IF_CONS :: Instr (a ': 'TList a ': s) s'
          -> Instr s s'
          -> Instr ('TList a ': s) s'

data Sing (t :: T) where
  STInt  :: Sing 'TInt
  STNat  :: Sing 'TNat
  STList :: Typeable t => Sing t -> Sing ('TList t)
  STPair :: (Typeable p, Typeable q)
         => Sing p -> Sing q -> Sing ('TPair p q)

fromSing :: Sing t -> T
fromSing STInt = TInt
fromSing STNat = TNat
fromSing (STList t) = TList (fromSing t)
fromSing (STPair p q) = TPair (fromSing p) (fromSing q)

data SomeSing where
  SomeSing :: Typeable t => Sing t -> SomeSing

withSomeSing
  :: SomeSing
  -> (forall t . Typeable t => Sing t -> a)
  -> a
withSomeSing (SomeSing a) f = f a

toSing :: T -> SomeSing
toSing TInt = SomeSing STInt
toSing TNat = SomeSing STNat
toSing (TList t) =
  withSomeSing (toSing t) $ SomeSing . STList
toSing (TPair p q) =
  withSomeSing (toSing p) $ \p' ->
  withSomeSing (toSing q) $ \q' ->
    SomeSing (STPair p' q')

typeCheckVal :: Sing t -> UVal -> Maybe (Val t)
typeCheckVal STInt (UInt i) = Just (VInt i)
typeCheckVal STNat (UInt i) = do
  guard (i >= 0)
  pure (VNat $ fromIntegral i)
typeCheckVal (STPair pt qt) (UPair p q) = do
  pv <- typeCheckVal pt p
  qv <- typeCheckVal qt q
  pure $ VPair pv qv
typeCheckVal (STList t) (UList l) =
  VList <$> mapM (typeCheckVal t) l
typeCheckVal _ _ = Nothing

data Stack inp where
  SNil  :: Stack '[]
  (::&) :: (Typeable s, Typeable a)
        => Sing a -> Stack s -> Stack (a ': s)
infixr 7 ::&

data SomeInstr inp where
  (:::) :: Typeable out
        => Instr inp out -> Stack out -> SomeInstr inp
infixr 5 :::

typeCheckI
  :: Typeable inp => Stack inp -> UInstr -> Maybe (SomeInstr inp)
typeCheckI s UNOP = pure (Nop ::: s)
typeCheckI (_ ::& s) UDROP = pure (DROP ::: s)
typeCheckI (a ::& s) UDUP = pure (DUP ::: a ::& a ::& s)
typeCheckI (a ::& b ::& s) USWAP = pure (SWAP ::: b ::& a ::& s)
typeCheckI s (UPUSH t v) = withSomeSing (toSing t) $ \t' -> do
  val <- typeCheckVal t' v
  pure (PUSH val ::: t' ::& s)
typeCheckI (a ::& b ::& s) UPAIR = pure (PAIR ::: STPair a b ::& s)
typeCheckI (STPair a _ ::& s) UCAR = pure (CAR ::: a ::& s)
typeCheckI (STPair _ b ::& s) UCDR = pure (CDR ::: b ::& s)
typeCheckI (STInt ::& STInt ::& s) UADD = pure (ADDii ::: STInt ::& s)
typeCheckI (STNat ::& STNat ::& s) UADD = pure (ADDnn ::: STNat ::& s)
typeCheckI (STInt ::& STNat ::& s) UADD = pure (ADDin ::: STInt ::& s)
typeCheckI (STNat ::& STInt ::& s) UADD = pure (ADDni ::: STInt ::& s)
typeCheckI s (UNIL t) = withSomeSing (toSing t) $ \t' ->
  pure (NIL ::: STList t' ::& s)
typeCheckI ((_ :: Sing a) ::& STList (e :: Sing b) ::& s) UCONS = do
  Refl <- eqT @a @b
  pure (CONS ::: STList e ::& s)
typeCheckI (STList a ::& s) (UIF_CONS consCase nilCase) = do
  nc ::: (s' :: Stack out1) <- typeCheck s nilCase
  cc ::: (_ :: Stack out2) <- typeCheck (a ::& STList a ::& s) consCase
  Refl <- eqT @out1 @out2
  pure (IF_CONS cc nc ::: s')
typeCheckI _ _ = Nothing

typeCheck
  :: Typeable inp => Stack inp -> [UInstr] -> Maybe (SomeInstr inp)
typeCheck s [] = pure (Nop ::: s)
typeCheck s (i : []) = typeCheckI s i
typeCheck s (i : is) = do
  a ::: s' <- typeCheckI s i
  b ::: s'' <- typeCheck s' is
  pure (a `Seq` b ::: s'')

interpret
  :: Rec Val inp -> Instr inp out -> Rec Val out
interpret s Nop = s
interpret s (Seq a b) = interpret (interpret s a) b
interpret (_ :& s) DROP = s
interpret (a :& s) DUP = a :& a :& s
interpret (a :& b :& s) SWAP = b :& a :& s
interpret (a :& b :& s) PAIR = VPair a b :& s
interpret s (PUSH v) = v :& s
interpret (VPair a _ :& s) CAR = a :& s
interpret (VPair _ b :& s) CDR = b :& s
interpret (VInt a :& VInt b :& s) ADDii = VInt (a + b) :& s
interpret (VInt a :& VNat b :& s) ADDin = VInt (a + fromIntegral b) :& s
interpret (VNat a :& VInt b :& s) ADDni = VInt (fromIntegral a + b) :& s
interpret (VNat a :& VNat b :& s) ADDnn = VNat (a + b) :& s
interpret s NIL = VList [] :& s
interpret (a :& VList l :& s) CONS = VList (a : l) :& s
interpret (VList [] :& s) (IF_CONS _ nilCase) = interpret s nilCase
interpret (VList (a : l) :& s) (IF_CONS consCase _) =
  interpret (a :& VList l :& s) consCase
