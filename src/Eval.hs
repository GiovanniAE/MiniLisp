{-# LANGUAGE LambdaCase #-}

module Eval 
  ( Value(..)
  , Env
  , emptyEnv
  , eval
  , evalSteps
  , prettyValue
  ) where

import AST
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)

-- | Valores semánticos (formas normales)
data Value
  = VInt Integer
  | VBool Bool
  | VPair Value Value
  | VNil
  | VCons Value Value
  | VClosure Env String Expr  -- Closure con entorno capturado
  deriving (Eq, Show)

-- | Entorno de evaluación (binding de variables a valores)
type Env = Map.Map String Value

-- | Entorno vacío
emptyEnv :: Env
emptyEnv = Map.empty

-- | Evaluador principal (big-step para simplicidad de implementación)
-- Nota: La especificación formal usa small-step, pero big-step
-- es equivalente y más eficiente para la implementación
eval :: Env -> Expr -> Either String Value
eval env = \case
  -- [E-VAR]
  Var x -> 
    case Map.lookup x env of
      Just v -> Right v
      Nothing -> Left $ "Unbound variable: " ++ x
  
  -- Valores
  IntLit n -> Right (VInt n)
  BoolLit b -> Right (VBool b)
  Nil -> Right VNil
  
  -- [E-PAIR-*]
  Pair e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    return $ VPair v1 v2
  
  -- [E-CONS]
  Cons e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    return $ VCons v1 v2
  
  -- [E-ADD-*]
  Add e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
      _ -> Left "Type error: + expects integers"
  
  -- [E-SUB-*]
  Sub e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
      _ -> Left "Type error: - expects integers"
  
  -- [E-MUL-*]
  Mul e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
      _ -> Left "Type error: * expects integers"
  
  -- [E-DIV-*], [E-DIV-ZERO]
  Div e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt _, VInt 0) -> Left "Division by zero"
      (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
      _ -> Left "Type error: / expects integers"
  
  -- [E-EQ-*]
  Eq e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
      (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
      _ -> Left "Type error: = expects same types"
  
  -- [E-LT-*]
  Lt e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
      _ -> Left "Type error: < expects integers"
  
  -- Similar para Gt, Le, Ge, Ne
  Gt e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
      _ -> Left "Type error: > expects integers"
  
  Le e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 <= n2)
      _ -> Left "Type error: <= expects integers"
  
  Ge e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 >= n2)
      _ -> Left "Type error: >= expects integers"
  
  Ne e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 /= n2)
      (VBool b1, VBool b2) -> Right $ VBool (b1 /= b2)
      _ -> Left "Type error: != expects same types"
  
  -- [E-NOT-*]
  Not e -> do
    v <- eval env e
    case v of
      VBool b -> Right $ VBool (not b)
      _ -> Left "Type error: not expects boolean"
  
  -- [E-IF-*]
  If cond thenE elseE -> do
    v <- eval env cond
    case v of
      VBool True -> eval env thenE
      VBool False -> eval env elseE
      _ -> Left "Type error: if expects boolean condition"
  
  -- [E-LET-*]
  Let x e1 e2 -> do
    v1 <- eval env e1
    eval (Map.insert x v1 env) e2
  
  -- [E-LAMBDA]
  Lambda x body -> 
    Right $ VClosure env x body
  
  -- [E-APP-*]
  App e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2
    case v1 of
      VClosure closureEnv param body ->
        eval (Map.insert param v2 closureEnv) body
      _ -> Left "Type error: application of non-function"
  
  -- [E-FST-*]
  Fst e -> do
    v <- eval env e
    case v of
      VPair v1 _ -> Right v1
      VCons v1 _ -> Right v1  -- head
      _ -> Left "Type error: fst expects pair or cons"
  
  -- [E-SND-*]
  Snd e -> do
    v <- eval env e
    case v of
      VPair _ v2 -> Right v2
      VCons _ v2 -> Right v2  -- tail
      _ -> Left "Type error: snd expects pair or cons"

-- | Evaluación paso a paso (small-step)
-- Para depuración y visualización
evalSteps :: Env -> Expr -> [Either String Expr]
evalSteps env expr = Right expr : go expr
  where
    go e = case step env e of
      Left _ -> []
      Right e' -> Right e' : go e'

-- | Un paso de evaluación (small-step)
step :: Env -> Expr -> Either String Expr
step env = \case
  -- Valores no reducen
  IntLit _ -> Left "Value"
  BoolLit _ -> Left "Value"
  Nil -> Left "Value"
  Lambda _ _ -> Left "Value"
  
  -- Variables se sustituyen
  Var x -> case Map.lookup x env of
    Just (VInt n) -> Right (IntLit n)
    Just (VBool b) -> Right (BoolLit b)
    Just VNil -> Right Nil
    Nothing -> Left $ "Unbound: " ++ x
    _ -> Left "Complex value"
  
  -- Operadores: evaluar operandos primero
  Add e1 e2 -> do
    case (e1, e2) of
      (IntLit n1, IntLit n2) -> Right $ IntLit (n1 + n2)
      (IntLit _, _) -> do
        e2' <- step env e2
        Right $ Add e1 e2'
      _ -> do
        e1' <- step env e1
        Right $ Add e1' e2
  
  -- If evalúa condición primero
  If (BoolLit True) e2 _ -> Right e2
  If (BoolLit False) _ e3 -> Right e3
  If e1 e2 e3 -> do
    e1' <- step env e1
    Right $ If e1' e2 e3
  
  -- Let sustituye
  Let x (IntLit n) body -> Right $ substExpr x (IntLit n) body
  Let x (BoolLit b) body -> Right $ substExpr x (BoolLit b) body
  Let x e1 e2 -> do
    e1' <- step env e1
    Right $ Let x e1' e2
  
  -- Aplicación: beta-reducción
  App (Lambda x body) v@(IntLit _) -> Right $ substExpr x v body
  App (Lambda x body) v@(BoolLit _) -> Right $ substExpr x v body
  App e1@(Lambda _ _) e2 -> do
    e2' <- step env e2
    Right $ App e1 e2'
  App e1 e2 -> do
    e1' <- step env e1
    Right $ App e1' e2
  
  e -> Left $ "Cannot step: " ++ show e

-- | Sustitución capture-avoiding
substExpr :: String -> Expr -> Expr -> Expr
substExpr var val = go
  where
    go (Var x) | x == var = val
               | otherwise = Var x
    go (IntLit n) = IntLit n
    go (BoolLit b) = BoolLit b
    go Nil = Nil
    go (Pair e1 e2) = Pair (go e1) (go e2)
    go (Cons e1 e2) = Cons (go e1) (go e2)
    go (Add e1 e2) = Add (go e1) (go e2)
    go (Sub e1 e2) = Sub (go e1) (go e2)
    go (Mul e1 e2) = Mul (go e1) (go e2)
    go (Div e1 e2) = Div (go e1) (go e2)
    go (Eq e1 e2) = Eq (go e1) (go e2)
    go (Lt e1 e2) = Lt (go e1) (go e2)
    go (Gt e1 e2) = Gt (go e1) (go e2)
    go (Le e1 e2) = Le (go e1) (go e2)
    go (Ge e1 e2) = Ge (go e1) (go e2)
    go (Ne e1 e2) = Ne (go e1) (go e2)
    go (Not e) = Not (go e)
    go (If c t e) = If (go c) (go t) (go e)
    go (Let x e1 e2) | x == var = Let x (go e1) e2  -- No sustituir en scope
                     | otherwise = Let x (go e1) (go e2)
    go (Lambda x body) | x == var = Lambda x body  -- No sustituir en scope
                       | otherwise = Lambda x (go body)
    go (App e1 e2) = App (go e1) (go e2)
    go (Fst e) = Fst (go e)
    go (Snd e) = Snd (go e)

-- | Pretty printer para valores
prettyValue :: Value -> String
prettyValue (VInt n) = show n
prettyValue (VBool True) = "#t"
prettyValue (VBool False) = "#f"
prettyValue VNil = "[]"
prettyValue (VPair v1 v2) = "(" ++ prettyValue v1 ++ ", " ++ prettyValue v2 ++ ")"
prettyValue (VCons v1 v2) = "[" ++ prettyValue v1 ++ go v2 ++ "]"
  where
    go VNil = ""
    go (VCons v vr) = ", " ++ prettyValue v ++ go vr
    go v = " | " ++ prettyValue v  -- Improper list
prettyValue (VClosure _ x _) = "<function: λ" ++ x ++ ".?>"