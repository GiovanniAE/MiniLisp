{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module AST 
  ( Expr(..)
  , prettyExpr
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Sintaxis Abstracta del Núcleo de MiniLisp
-- Todas las construcciones aquí son primitivas
data Expr
  -- Valores atómicos
  = Var String              -- Variable
  | IntLit Integer          -- Literal entera
  | BoolLit Bool            -- Literal booleana
  
  -- Estructuras de datos
  | Pair Expr Expr          -- Par ordenado (e1, e2)
  | Nil                     -- Lista vacía
  | Cons Expr Expr          -- Constructor de lista
  
  -- Operadores aritméticos binarios
  | Add Expr Expr           -- Suma
  | Sub Expr Expr           -- Resta
  | Mul Expr Expr           -- Multiplicación
  | Div Expr Expr           -- División
  
  -- Operadores de comparación binarios
  | Eq Expr Expr            -- Igualdad
  | Lt Expr Expr            -- Menor que
  | Gt Expr Expr            -- Mayor que
  | Le Expr Expr            -- Menor o igual
  | Ge Expr Expr            -- Mayor o igual
  | Ne Expr Expr            -- Diferente
  
  -- Operadores lógicos
  | Not Expr                -- Negación
  
  -- Control de flujo
  | If Expr Expr Expr       -- Condicional
  
  -- Binding
  | Let String Expr Expr    -- Definición local
  
  -- Funciones (currificadas)
  | Lambda String Expr      -- Función anónima
  | App Expr Expr           -- Aplicación
  
  -- Proyecciones
  | Fst Expr                -- Primera proyección
  | Snd Expr                -- Segunda proyección
  
  deriving (Eq, Show, Generic, NFData)

-- | Pretty printer para expresiones del núcleo
prettyExpr :: Expr -> String
prettyExpr = go 0
  where
    indent n = replicate (n * 2) ' '
    
    go _ (Var x) = x
    go _ (IntLit n) = show n
    go _ (BoolLit True) = "#t"
    go _ (BoolLit False) = "#f"
    go _ Nil = "nil"
    
    go n (Pair e1 e2) = 
      "(" ++ go n e1 ++ ", " ++ go n e2 ++ ")"
    
    go n (Cons e1 e2) = 
      "(cons " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Add e1 e2) = 
      "(+ " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Sub e1 e2) = 
      "(- " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Mul e1 e2) = 
      "(* " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Div e1 e2) = 
      "(/ " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Eq e1 e2) = 
      "(= " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Lt e1 e2) = 
      "(< " ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Not e) = 
      "(not " ++ go n e ++ ")"
    
    go n (If c t e) =
      "(if " ++ go n c ++ "\n" ++
      indent (n+1) ++ go (n+1) t ++ "\n" ++
      indent (n+1) ++ go (n+1) e ++ ")"
    
    go n (Let x e1 e2) =
      "(let " ++ x ++ " " ++ go n e1 ++ "\n" ++
      indent (n+1) ++ go (n+1) e2 ++ ")"
    
    go n (Lambda x body) =
      "(lambda (" ++ x ++ ") " ++ go n body ++ ")"
    
    go n (App e1 e2) =
      "(" ++ go n e1 ++ " " ++ go n e2 ++ ")"
    
    go n (Fst e) = "(fst " ++ go n e ++ ")"
    go n (Snd e) = "(snd " ++ go n e ++ ")"
    go n e = show e