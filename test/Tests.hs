{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Test.Hspec
import Parser
import Desugar
import Eval
import AST
import SurfaceAST

main :: IO ()
main = hspec $ do
  describe "Parser Tests" $ do
    it "parsea números enteros" $ do
      parseExpr "42" `shouldBe` Right (SInt 42)
      parseExpr "-17" `shouldBe` Right (SInt (-17))
    
    it "parsea booleanos" $ do
      parseExpr "#t" `shouldBe` Right (SBool True)
      parseExpr "#f" `shouldBe` Right (SBool False)
    
    it "parsea variables" $ do
      parseExpr "x" `shouldBe` Right (SVar "x")
      parseExpr "my-var" `shouldBe` Right (SVar "my-var")
    
    it "parsea operadores variádicos" $ do
      parseExpr "(+ 1 2 3)" `shouldBe` 
        Right (SAdd [SInt 1, SInt 2, SInt 3])
    
    it "parsea listas" $ do
      parseExpr "[1, 2, 3]" `shouldBe` 
        Right (SList [SInt 1, SInt 2, SInt 3])
      parseExpr "[]" `shouldBe` Right (SList [])
    
    it "parsea pares" $ do
      parseExpr "(1, 2)" `shouldBe` 
        Right (SPair (SInt 1) (SInt 2))
    
    it "parsea if" $ do
      parseExpr "(if #t 1 2)" `shouldBe`
        Right (SIf (SBool True) (SInt 1) (SInt 2))
    
    it "parsea lambda" $ do
      parseExpr "(lambda (x y) (+ x y))" `shouldBe`
        Right (SLambda ["x", "y"] (SAdd [SVar "x", SVar "y"]))
    
    it "parsea let" $ do
      parseExpr "(let ((x 5)) x)" `shouldBe`
        Right (SLet [("x", SInt 5)] (SVar "x"))
  
  describe "Desugar Tests" $ do
    it "desazucariza add1" $ do
      desugar (SAdd1 (SInt 5)) `shouldBe`
        Add (IntLit 5) (IntLit 1)
    
    it "desazucariza sub1" $ do
      desugar (SSub1 (SInt 10)) `shouldBe`
        Sub (IntLit 10) (IntLit 1)
    
    it "desazucariza operadores variádicos" $ do
      desugar (SAdd [SInt 1, SInt 2, SInt 3]) `shouldBe`
        Add (IntLit 1) (Add (IntLit 2) (IntLit 3))
    
    it "desazucariza if0" $ do
      desugar (SIf0 (SInt 0) (SInt 1) (SInt 2)) `shouldBe`
        If (Eq (IntLit 0) (IntLit 0)) (IntLit 1) (IntLit 2)
    
    it "desazucariza lambda currificada" $ do
      desugar (SLambda ["x", "y"] (SVar "x")) `shouldBe`
        Lambda "x" (Lambda "y" (Var "x"))
    
    it "desazucariza listas a cons" $ do
      desugar (SList [SInt 1, SInt 2]) `shouldBe`
        Cons (IntLit 1) (Cons (IntLit 2) Nil)
    
    it "desazucariza head y tail" $ do
      desugar (SHead (SVar "lst")) `shouldBe` Fst (Var "lst")
      desugar (STail (SVar "lst")) `shouldBe` Snd (Var "lst")
  
  describe "Eval Tests" $ do
    it "evalúa números" $ do
      eval emptyEnv (IntLit 42) `shouldBe` Right (VInt 42)
    
    it "evalúa booleanos" $ do
      eval emptyEnv (BoolLit True) `shouldBe` Right (VBool True)
    
    it "evalúa suma" $ do
      eval emptyEnv (Add (IntLit 2) (IntLit 3)) `shouldBe` 
        Right (VInt 5)
    
    it "evalúa multiplicación" $ do
      eval emptyEnv (Mul (IntLit 4) (IntLit 5)) `shouldBe` 
        Right (VInt 20)
    
    it "evalúa comparaciones" $ do
      eval emptyEnv (Lt (IntLit 1) (IntLit 2)) `shouldBe` 
        Right (VBool True)
      eval emptyEnv (Eq (IntLit 5) (IntLit 5)) `shouldBe` 
        Right (VBool True)
    
    it "evalúa if" $ do
      eval emptyEnv (If (BoolLit True) (IntLit 1) (IntLit 2)) `shouldBe`
        Right (VInt 1)
      eval emptyEnv (If (BoolLit False) (IntLit 1) (IntLit 2)) `shouldBe`
        Right (VInt 2)
    
    it "evalúa let" $ do
      eval emptyEnv (Let "x" (IntLit 5) (Add (Var "x") (IntLit 3))) `shouldBe`
        Right (VInt 8)
    
    it "evalúa lambda y aplicación" $ do
      let expr = App (Lambda "x" (Mul (Var "x") (Var "x"))) (IntLit 5)
      eval emptyEnv expr `shouldBe` Right (VInt 25)
    
    it "evalúa pares" $ do
      eval emptyEnv (Pair (IntLit 1) (IntLit 2)) `shouldBe`
        Right (VPair (VInt 1) (VInt 2))
    
    it "evalúa proyecciones" $ do
      let pair = Pair (IntLit 1) (IntLit 2)
      eval emptyEnv (Fst pair) `shouldBe` Right (VInt 1)
      eval emptyEnv (Snd pair) `shouldBe` Right (VInt 2)
    
    it "evalúa listas" $ do
      eval emptyEnv (Cons (IntLit 1) Nil) `shouldBe`
        Right (VCons (VInt 1) VNil)
    
    it "detecta división por cero" $ do
      eval emptyEnv (Div (IntLit 10) (IntLit 0)) `shouldSatisfy`
        (\case
          Left msg -> "zero" `elem` words (map toLower msg)
          Right _ -> False)
    
    it "detecta variable no definida" $ do
      eval emptyEnv (Var "undefined") `shouldSatisfy`
        (\case
          Left _ -> True
          Right _ -> False)
  
  describe "Integration Tests" $ do
    it "factorial de 5" $ do
      let code = "(letrec ((fact (lambda (n) \
                 \(if (<= n 1) 1 (* n (fact (- n 1))))))) \
                 \(fact 5))"
      case parseExpr code of
        Right sexpr -> do
          let core = desugar sexpr
          eval emptyEnv core `shouldBe` Right (VInt 120)
        Left err -> expectationFailure $ "Parse error: " ++ err
    
    it "suma de primeros 10 naturales" $ do
      let code = "(letrec ((sum (lambda (n) \
                 \(if0 n 0 (+ n (sum (sub1 n))))))) \
                 \(sum 10))"
      case parseExpr code of
        Right sexpr -> do
          let core = desugar sexpr
          eval emptyEnv core `shouldBe` Right (VInt 55)
        Left err -> expectationFailure $ "Parse error: " ++ err
    
    it "fibonacci de 10" $ do
      let code = "(letrec ((fib (lambda (n) \
                 \(cond [(= n 0) 0] [(= n 1) 1] \
                 \[else (+ (fib (- n 1)) (fib (- n 2)))])))) \
                 \(fib 10))"
      case parseExpr code of
        Right sexpr -> do
          let core = desugar sexpr
          eval emptyEnv core `shouldBe` Right (VInt 55)
        Left err -> expectationFailure $ "Parse error: " ++ err
    
    it "operador variádico complejo" $ do
      let code = "(+ 1 2 (* 3 4) (- 10 5))"
      case parseExpr code of
        Right sexpr -> do
          let core = desugar sexpr
          eval emptyEnv core `shouldBe` Right (VInt 20)
        Left err -> expectationFailure $ "Parse error: " ++ err

-- Helper para comparación case-insensitive
toLower :: Char -> Char
toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
          | otherwise = c