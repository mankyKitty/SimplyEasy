{-# LANGUAGE OverloadedStrings #-}
module SimplyTyped where

import Data.Text (Text)
import Control.Monad (unless)
import Control.Monad.Trans.Error (throwError)

import qualified Data.Text as T

data Name
  = Const Text
  | Bound Int
  | Unquoted Int
  deriving (Show, Eq)

data Inferable
  = Ann Checkable Type
  | Var Int
  | Par Name
  | Inferable :@: Checkable
  deriving (Show, Eq)

data Checkable
  = Inf Inferable
  | Lam Checkable
  deriving (Show, Eq)

data Type
  = TPar Name
  | Fun Type Type
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NPar Name
  | NApp Neutral Value

data Kind = Star deriving (Show)
data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name,Info)]
type Env = [Value]
type Result a = Either String a

evalInf :: Inferable -> Env -> Value
evalInf (Ann e _)   d = evalChk e d
evalInf (Par x)     d = vpar x
evalInf (Var i)     d = d !! i
evalInf (e1 :@: e2) d = vapp (evalInf e1 d) (evalChk e2 d)

evalChk :: Checkable -> Env -> Value
evalChk (Inf i) d = evalInf i d
evalChk (Lam e) d = VLam (\x -> evalChk e (x : d))

vapp :: Value -> Value -> Value
vapp (VLam f)      v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

vpar :: Name -> Value
vpar n = VNeutral (NPar n)

kindOut :: Context -> Type -> Kind -> Result ()
kindOut c (TPar x) Star = case lookup x c of
  Just (HasKind Star) -> return ()
  Nothing             -> Left "unknown identifier"
kindOut c (Fun k k') Star = do kindOut c k Star
                               kindOut c k' Star

typeInf0 :: Context -> Inferable -> Result Type
typeInf0 = typeInf 0

typeInf :: Int -> Context -> Inferable -> Result Type
typeInf i c (Ann e t) = do kindOut c t Star
                           typeOut i c e t
                           return t
typeInf i c (Par x) = case lookup x c of
  Just (HasType t) -> return t
  Nothing          -> Left "unknown identifier"
typeInf i c (e1 :@: e2) = do a <- typeInf i c e1
                             case a of
                               Fun t t' -> do typeOut i c e2 t
                                              return t'
                               _        -> Left "illegal application"

typeOut :: Int -> Context -> Checkable -> Type -> Result ()
typeOut i c (Inf e) t = do t' <- typeInf i c e
                           unless (t == t') (Left "type mismatch")
typeOut i c (Lam e) (Fun t t') = typeOut
                                (i + 1)
                                ((Bound i, HasType t) : c)
                                (substOut 0 (Par (Bound i)) e) t'
typeOut i c _ _ = Left "type mismatch"
                                
substInf :: Int -> Inferable -> Inferable -> Inferable
substInf i r (Ann e t) = Ann (substOut i r e) t
substInf i r (Var j) = if i == j then r else Var j
substInf i r (Par y) = Par y
substInf i r (e1 :@: e2) = substInf i r e1 :@: substOut i r e2

substOut :: Int -> Inferable -> Checkable -> Checkable
substOut i r (Inf e) = Inf (substInf i r e)
substOut i r (Lam e) = Lam (substOut (i + 1) r e)

quote0 :: Value -> Checkable
quote0 = quote 0

quote :: Int -> Value -> Checkable
quote i (VLam f) = Lam (quote (i + 1) (f (vpar (Unquoted i)))) 
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> Inferable
neutralQuote i (NPar x) = varpar i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

varpar :: Int -> Name -> Inferable
varpar i (Unquoted k) = Var (i - k - 1)
varpar i x            = Par x

id' = Lam (Inf (Var 0))
const' = Lam (Lam (Inf (Var 1)))
tpar a = TPar (Const a)
par x = Inf (Par (Const x))
term1 = Ann id' (Fun (tpar "a") (tpar "a")) :@: par "y"
term2 = Ann const' (Fun (Fun (tpar "b") (tpar "b"))
                   (Fun (tpar "a")
                        (Fun (tpar "b") (tpar "b"))))
        :@: id' :@: par "y"

env1 = [(Const "y", HasType (tpar "a")),
        (Const "a", HasKind Star)]
env2 = [(Const "b", HasKind Star)] ++ env1
