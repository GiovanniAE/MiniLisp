{-# LANGUAGE DeriveGeneric #-}

module SurfaceAST 
  ( SExpr(..)
  , Binding
  ) where

import GHC.Generics (Generic)

type Binding = (String, SExpr)

-- | Sintaxis Abstracta de Superficie
-- Incluye todas las extensiones y azúcar sintáctica
data SExpr
  -- Valores atómicos
  = SVar String
  | SInt Integer
  | SBool Bool
  
  -- Estructuras de datos
  | SPair SExpr SExpr
  | SList [SExpr]
  
  -- Operadores aritméticos variádicos
  | SAdd [SExpr]            -- (+ e1 e2 ... en)
  | SSub [SExpr]            -- (- e1 e2 ... en)
  | SMul [SExpr]            -- (* e1 e2 ... en)
  | SDiv [SExpr]            -- (/ e1 e2 ... en)
  
  -- Operadores aritméticos unarios
  | SAdd1 SExpr             -- (add1 e)
  | SSub1 SExpr             -- (sub1 e)
  | SSqrt SExpr             -- (sqrt e)
  
  -- Operador de potencia
  | SExpt SExpr SExpr       -- (expt base exp)
  
  -- Operadores de comparación variádicos
  | SEq [SExpr]             -- (= e1 e2 ... en)
  | SLt [SExpr]             -- (< e1 e2 ... en)
  | SGt [SExpr]             -- (> e1 e2 ... en)
  | SLe [SExpr]             -- (<= e1 e2 ... en)
  | SGe [SExpr]             -- (>= e1 e2 ... en)
  | SNe [SExpr]             -- (!= e1 e2 ... en)
  
  -- Operadores lógicos
  | SNot SExpr              -- (not e)
  
  -- Condicionales
  | SIf SExpr SExpr SExpr   -- (if cond then else)
  | SIf0 SExpr SExpr SExpr  -- (if0 e then else)
  | SCond [(SExpr, SExpr)] SExpr  -- (cond [g1 e1] ... [else ee])
  
  -- Bindings
  | SLet [Binding] SExpr         -- (let ((x1 e1) ...) body)
  | SLetStar [Binding] SExpr     -- (let* ((x1 e1) ...) body)
  | SLetRec [Binding] SExpr      -- (letrec ((x1 e1) ...) body)
  
  -- Funciones
  | SLambda [String] SExpr       -- (lambda (x1 ... xn) body)
  | SApp SExpr [SExpr]           -- (f e1 e2 ... en)
  
  -- Proyecciones y operaciones sobre listas
  | SFst SExpr              -- (fst e)
  | SSnd SExpr              -- (snd e)
  | SHead SExpr             -- (head e)
  | STail SExpr             -- (tail e)
  
  deriving (Eq, Show, Generic)