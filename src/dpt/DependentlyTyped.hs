{-# LANGUAGE OverloadedStrings #-}
module DependentlyTyped where

import Data.Text (Text(..))
import Control.Monad (unless)
import Control.Monad.Trans.Error (throwError)

data Name
  = Const Text
  | Bound Int
  | Unquoted Int
  deriving (Show, Eq)

data TermIn
  = Ann TermOut TermOut
  | Star
  | Pi TermOut TermOut
  | Var Int
  | Par Name
  | TermIn :@: TermOut
  deriving (Show, Eq)

data TermOut
  = Inf TermIn
  | Lam TermOut
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NPar Name
  | NApp Neutral Value

type Type = Value
type Context = [(Name, Type)]
type Env = [Value]

type Result a = Either String a

evalIn :: TermIn -> Env -> Value
evalIn (Ann e _)   d = evalOut e d
evalIn Star        d = VStar
evalIn (Pi t t')   d = VPi (evalOut t d) (\x -> evalOut t' (x : d))
evalIn (Par x)     d = vpar x
evalIn (Var i)     d = d !! i
evalIn (e1 :@: e2) d = vapp (evalIn e1 d) (evalOut e2 d)

evalOut :: TermOut -> Env -> Value
evalOut (Inf i) d = evalIn i d
evalOut (Lam e) d = VLam (\x -> evalOut e (x : d))

vapp :: Value -> Value -> Value
vapp (VLam f)      v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

vpar :: Name -> Value
vpar n = VNeutral (NPar n)

typeIn0 :: Context -> TermIn -> Result Type
typeIn0 = typeIn 0

typeIn :: Int -> Context -> TermIn -> Result Type
typeIn i c (Ann e t) = do typeOut i c t VStar
                          let v = evalOut t []
                          typeOut i c e v
                          return v
typeIn i c Star = return VStar
typeIn i c (Pi t t') = do typeOut i c t VStar
                          let v = evalOut t []
                          typeOut (i + 1) ((Bound i,v) : c)
                            (substOut 0 (Par (Bound i)) t') VStar
                          return VStar
typeIn i c (Par x) = case lookup x c of
  Just v  -> return v
  Nothing -> Left "unknown identifier"
typeIn i c (e1 :@: e2) = do a <- typeIn i c e1
                            case a of
                              VPi v f -> do typeOut i c e2 v
                                            return (f (evalOut e2 []))
                              _        -> Left "illegal application"

typeOut :: Int -> Context -> TermOut -> Type -> Result ()
typeOut i c (Inf e) v = do v' <- typeIn i c e
                           unless (quote0 v == quote0 v') (Left "type mismatch")
typeOut i c (Lam e) (VPi v f) = typeOut
                                (i + 1)
                                ((Bound i, v) : c)
                                (substOut 0 (Par (Bound i)) e) (f (vpar (Bound i)))
typeOut i c _ _ = Left "type mismatch"
                                
substIn :: Int -> TermIn -> TermIn -> TermIn 
substIn i r (Ann e t) = Ann (substOut i r e) t
substIn i r Star = Star
substIn i r (Pi t t') = Pi (substOut i r t) (substOut (i + 1) r t')
substIn i r (Var j) = if i == j then r else Var j
substIn i r (Par y) = Par y
substIn i r (e1 :@: e2) = substIn i r e1 :@: substOut i r e2

substOut :: Int -> TermIn -> TermOut -> TermOut
substOut i r (Inf e) = Inf (substIn i r e)
substOut i r (Lam e) = Lam (substOut (i + 1) r e)

quote0 :: Value -> TermOut
quote0 = quote 0

quote :: Int -> Value -> TermOut
quote i (VLam f) = Lam (quote (i + 1) (f (vpar (Unquoted i)))) 
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> TermIn
neutralQuote i (NPar x) = varpar i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

varpar :: Int -> Name -> TermIn
varpar i (Unquoted k) = Var (i - k - 1)
varpar i x            = Par x
