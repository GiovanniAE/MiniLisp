module Desugar 
  ( desugar
  , desugarOp
  ) where

import AST
import SurfaceAST
import qualified Data.List as L

-- | Función principal de desazucarización
-- Implementa todas las reglas [DS-*] definidas formalmente
desugar :: SExpr -> Expr
desugar se = case se of
  -- [DS-VAR], [DS-INT], [DS-BOOL]
  SVar x -> Var x
  SInt n -> IntLit n
  SBool b -> BoolLit b
  
  -- [DS-PAIR]
  SPair e1 e2 -> Pair (desugar e1) (desugar e2)
  
  -- [DS-NIL], [DS-LIST-*]
  SList [] -> Nil
  SList [e] -> Cons (desugar e) Nil
  SList (e:es) -> Cons (desugar e) (desugar (SList es))
  
  -- Operadores aritméticos variádicos
  SAdd es -> desugarOp "+" es
  SSub es -> desugarOp "-" es
  SMul es -> desugarOp "*" es
  SDiv es -> desugarOp "/" es
  
  -- [DS-ADD1], [DS-SUB1]
  SAdd1 e -> Add (desugar e) (IntLit 1)
  SSub1 e -> Sub (desugar e) (IntLit 1)
  
  -- [DS-SQRT] - implementado como expt e 0.5
  -- Para enteros, usamos aproximación
  SSqrt e -> desugarSqrt (desugar e)
  
  -- [DS-EXPT]
  SExpt base exp -> desugarExpt (desugar base) (desugar exp)
  
  -- Operadores de comparación variádicos
  SEq es -> desugarComp Eq es
  SLt es -> desugarComp Lt es
  SGt es -> desugarComp Gt es
  SLe es -> desugarComp Le es
  SGe es -> desugarComp Ge es
  SNe es -> desugarComp Ne es
  
  -- [DS-NOT]
  SNot e -> Not (desugar e)
  
  -- [DS-IF]
  SIf c t e -> If (desugar c) (desugar t) (desugar e)
  
  -- [DS-IF0]
  SIf0 e t f -> If (Eq (desugar e) (IntLit 0)) 
                   (desugar t) 
                   (desugar f)
  
  -- [DS-COND-*]
  SCond clauses elseExpr -> desugarCond clauses elseExpr
  
  -- [DS-LET-*]
  SLet bindings body -> desugarLet bindings body
  
  -- [DS-LETSTAR-*]
  SLetStar bindings body -> desugarLetStar bindings body
  
  -- [DS-LETREC]
  SLetRec bindings body -> desugarLetRec bindings body
  
  -- [DS-LAMBDA-*]
  SLambda params body -> curryLambda params (desugar body)
  
  -- [DS-APP-*]
  SApp f args -> applyMany (desugar f) (map desugar args)
  
  -- [DS-FST], [DS-SND]
  SFst e -> Fst (desugar e)
  SSnd e -> Snd (desugar e)
  
  -- [DS-HEAD], [DS-TAIL]
  SHead e -> Fst (desugar e)  -- head = fst (cons)
  STail e -> Snd (desugar e)  -- tail = snd (cons)

-- | Desazucariza operadores aritméticos variádicos
-- [DS-ADD-BIN], [DS-ADD-VAR]
desugarOp :: String -> [SExpr] -> Expr
desugarOp op exprs = case exprs of
  [] -> error $ "Operator " ++ op ++ " requires at least 2 arguments"
  [_] -> error $ "Operator " ++ op ++ " requires at least 2 arguments"
  [e1, e2] -> applyBinOp op (desugar e1) (desugar e2)
  (e1:e2:rest) -> applyBinOp op 
                             (desugar e1) 
                             (desugarOp op (e2:rest))
  where
    applyBinOp "+" = Add
    applyBinOp "-" = Sub
    applyBinOp "*" = Mul
    applyBinOp "/" = Div
    applyBinOp _ = error "Unknown operator"

-- | Desazucariza operadores de comparación variádicos
-- [DS-EQ-VAR], [DS-LT-VAR], etc.
desugarComp :: (Expr -> Expr -> Expr) -> [SExpr] -> Expr
desugarComp _ [] = error "Comparison requires at least 2 arguments"
desugarComp _ [_] = error "Comparison requires at least 2 arguments"
desugarComp cons [e1, e2] = cons (desugar e1) (desugar e2)
desugarComp cons (e1:e2:rest) = 
  If (cons (desugar e1) (desugar e2))
     (desugarComp cons (e2:rest))
     (BoolLit False)

-- | Desazucariza cond a if anidados
-- [DS-COND-BASE], [DS-COND-REC]
desugarCond :: [(SExpr, SExpr)] -> SExpr -> Expr
desugarCond [] elseExpr = desugar elseExpr
desugarCond ((guard, body):rest) elseExpr =
  If (desugar guard)
     (desugar body)
     (desugarCond rest elseExpr)

-- | Desazucariza let a lambdas
-- [DS-LET-SINGLE], [DS-LET-MULTI]
desugarLet :: [Binding] -> SExpr -> Expr
desugarLet [] body = desugar body
desugarLet bindings body =
  let (vars, exprs) = unzip bindings
      lambda = foldr Lambda (desugar body) vars
      args = map desugar exprs
  in applyMany lambda args

-- | Desazucariza let* a let anidados
-- [DS-LETSTAR-BASE], [DS-LETSTAR-REC]
desugarLetStar :: [Binding] -> SExpr -> Expr
desugarLetStar [] body = desugar body
desugarLetStar [(x, e)] body = 
  Let x (desugar e) (desugar body)
desugarLetStar ((x, e):rest) body =
  Let x (desugar e) (desugarLetStar rest body)

-- | Desazucariza letrec usando combinador Y
-- [DS-LETREC]
desugarLetRec :: [Binding] -> SExpr -> Expr
desugarLetRec bindings body =
  -- Implementación de letrec usando el combinador Z (Y-combinator de call-by-value)
  -- letrec [(x1 e1) ... (xn en)] body
  -- => let x1 = Y (\x1. e1)
  --        x2 = Y (\x2. e2)
  --        ...
  --    in body
  let desugarBinding (x, e) restBody =
        Let x (fixCombinator x (desugar e)) restBody
  in foldr desugarBinding (desugar body) bindings
  where
    -- Combinador Z (Y-combinator para call-by-value)
    -- Z f = (λx. f (λv. x x v)) (λx. f (λv. x x v))
    fixCombinator :: String -> Expr -> Expr
    fixCombinator f funcBody =
      let zComb = Lambda "f"
                    (App (Lambda "x" 
                           (App (Var "f") 
                                (Lambda "v" (App (App (Var "x") (Var "x")) (Var "v")))))
                         (Lambda "x" 
                           (App (Var "f") 
                                (Lambda "v" (App (App (Var "x") (Var "x")) (Var "v"))))))
      in App zComb (Lambda f funcBody)

-- | Currifica lambdas multi-parámetro
-- [DS-LAMBDA-ZERO], [DS-LAMBDA-ONE], [DS-LAMBDA-CURRY]
curryLambda :: [String] -> Expr -> Expr
curryLambda [] body = Lambda "_" body  -- Unit lambda
curryLambda [x] body = Lambda x body
curryLambda (x:xs) body = Lambda x (curryLambda xs body)

-- | Aplica función a múltiples argumentos
-- [DS-APP-ZERO], [DS-APP-ONE], [DS-APP-MULTI]
applyMany :: Expr -> [Expr] -> Expr
applyMany f [] = App f (BoolLit True)  -- Unit application
applyMany f [e] = App f e
applyMany f (e:es) = applyMany (App f e) es

-- | Desazucariza sqrt (implementación simplificada para enteros)
desugarSqrt :: Expr -> Expr
desugarSqrt e = 
  -- sqrt(n) usando el método de Newton-Raphson
  -- Para simplificar, usamos una función auxiliar
  Let "x" e
    (Let "guess" (IntLit 1)
      (desugarSqrtHelper (Var "x") (Var "guess")))
  where
    desugarSqrtHelper x guess =
      -- if guess * guess = x then guess
      -- else sqrt_helper x ((guess + x/guess) / 2)
      If (Eq (Mul guess guess) x)
         guess
         (desugarSqrtHelper x 
           (Div (Add guess (Div x guess)) (IntLit 2)))

-- | Desazucariza expt (exponenciación)
desugarExpt :: Expr -> Expr -> Expr
desugarExpt base exp =
  If (Eq exp (IntLit 0))
     (IntLit 1)
     (If (Lt exp (IntLit 0))
         (error "Negative exponents not supported")
         (Mul base (desugarExpt base (Sub exp (IntLit 1)))))