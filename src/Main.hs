{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad (when, forever)
import Data.List (isPrefixOf)

import SurfaceAST
import Parser
import Desugar
import Eval
import AST (prettyExpr)

-- | REPL principal
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [filename] -> runFile filename
    _ -> putStrLn "Usage: minilisp [filename]"

-- | REPL interactivo
repl :: IO ()
repl = do
  putStrLn ""
  putStrLn "+============================================================+"
  putStrLn "|                                                            |"
  putStrLn "|                   Proyecto 1 - MiniLisp                    |"
  putStrLn "|                                                            |"
  putStrLn "|              Interprete Funcional en Haskell               |"
  putStrLn "|              UNAM - Facultad de Ciencias 2025              |"
  putStrLn "|                                                            |"
  putStrLn "+============================================================+"
  putStrLn ""
  putStrLn "+------------------------------------------------------------+"
  putStrLn "|  Comandos disponibles:                                     |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn "|  :salir            -> Salir del REPL                       |"
  putStrLn "|  :ayuda            -> Mostrar ayuda completa               |"
  putStrLn "|  :cargar <archivo> -> Cargar y ejecutar archivo            |"
  putStrLn "|  :nucleo <expr>    -> Ver nucleo desazucarizado            |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "Escribe una expresion Lisp para evaluarla..."
  putStrLn ""
  replLoop emptyEnv

replLoop :: Env -> IO ()
replLoop env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  
  case line of
    "" -> replLoop env
    
    ':':'s':'a':'l':'i':'r':_ -> do
      putStrLn ""
      putStrLn "+============================================================+"
      putStrLn "|                                                            |"
      putStrLn "|                       ¡Hasta pronto!                       |"
      putStrLn "|                                                            |"
      putStrLn "|                 Gracias por usar MiniLisp.                 |"
      putStrLn "|             Creado por Giovanni, Vania, Camila             |"
      putStrLn "|                                                            |"
      putStrLn "+============================================================+"
      putStrLn ""
      return ()
    
    ':':'a':'y':'u':'d':'a':_ -> do
      showHelp
      replLoop env
    
    ':':'c':'a':'r':'g':'a':'r':' ':filename -> do
      loadFile env filename >>= replLoop
    
    ':':'n':'u':'c':'l':'e':'o':' ':input -> do
      showCore input
      replLoop env
    
    input -> do
      newEnv <- evalAndPrint env input
      replLoop newEnv

-- | Evalúa e imprime resultado
evalAndPrint :: Env -> String -> IO Env
evalAndPrint env input = do
  case parseExpr input of
    Left err -> do
      putStrLn $ "[X] Error de sintaxis: " ++ err
      return env
    
    Right surfaceExpr -> do
      let coreExpr = desugar surfaceExpr
      
      case eval env coreExpr of
        Left err -> do
          putStrLn $ "[X] Error de evaluacion: " ++ err
          return env
        
        Right value -> do
          putStrLn $ "[OK] => " ++ prettyValue value
          return env

-- | Muestra la expresión del núcleo
showCore :: String -> IO ()
showCore input = 
  case parseExpr input of
    Left err -> putStrLn $ "[X] Error de parseo: " ++ err
    Right surfaceExpr -> do
      let coreExpr = desugar surfaceExpr
      putStrLn ""
      putStrLn "+--- AST Superficial ----------------------------------------+"
      print surfaceExpr
      putStrLn ""
      putStrLn "+--- AST Nucleo ---------------------------------------------+"
      print coreExpr
      putStrLn ""
      putStrLn "+--- Formato Pretty -----------------------------------------+"
      putStrLn $ prettyExpr coreExpr
      putStrLn "+------------------------------------------------------------+"
      putStrLn ""

-- | Carga y ejecuta archivo
loadFile :: Env -> FilePath -> IO Env
loadFile env filename = do
  content <- readFile filename
  case parseProgram content of
    Left err -> do
      putStrLn $ "[X] Error de parseo en " ++ filename ++ ": " ++ err
      return env
    
    Right exprs -> do
      putStrLn $ "[FILE] Cargando " ++ filename ++ "..."
      putStrLn ""
      execExprs env exprs
  where
    execExprs e [] = do
      putStrLn "[OK] Archivo cargado exitosamente!"
      putStrLn ""
      return e
    execExprs e (sexpr:rest) = do
      let coreExpr = desugar sexpr
      case eval e coreExpr of
        Left err -> do
          putStrLn $ "[X] Error: " ++ err
          return e
        Right value -> do
          putStrLn $ "  [OK] " ++ prettyValue value
          execExprs e rest

-- | Ejecuta archivo desde línea de comandos
runFile :: FilePath -> IO ()
runFile filename = do
  content <- readFile filename
  case parseProgram content of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right exprs -> mapM_ runExpr exprs
  where
    runExpr sexpr = do
      let coreExpr = desugar sexpr
      case eval emptyEnv coreExpr of
        Left err -> putStrLn $ "Error: " ++ err
        Right value -> putStrLn $ prettyValue value

-- | Muestra ayuda
showHelp :: IO ()
showHelp = do
  putStrLn ""
  putStrLn "+============================================================+"
  putStrLn "|                                                            |"
  putStrLn "|                     AYUDA DE MINILISP                      |"
  putStrLn "|                                                            |"
  putStrLn "+============================================================+"
  putStrLn ""
  putStrLn "+-- SINTAXIS BASICA -----------------------------------------+"
  putStrLn "|  Numeros:     42, -17, 0                                  |"
  putStrLn "|  Booleanos:   #t, #f                                      |"
  putStrLn "|  Variables:   x, foo, my-var                              |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "+-- OPERADORES ----------------------------------------------+"
  putStrLn "|  Aritmeticos: (+ 1 2 3), (- 10 5), (* 2 3 4), (/ 10 2)   |"
  putStrLn "|  Comparacion: (= 1 1), (< 1 2), (> 3 2), (<= 1 2)        |"
  putStrLn "|  Logicos:     (not #t)                                    |"
  putStrLn "|  Unarios:     (add1 5), (sub1 10), (sqrt 16)              |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "+-- ESTRUCTURAS DE DATOS ------------------------------------+"
  putStrLn "|  Pares:       (3, 5), (fst (1, 2)), (snd (1, 2))         |"
  putStrLn "|  Listas:      [1, 2, 3], (head [1,2,3]), (tail [1,2,3])  |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "+-- ESTRUCTURAS DE CONTROL ----------------------------------+"
  putStrLn "|  If:          (if (< x 0) (- x) x)                        |"
  putStrLn "|  If0:         (if0 x 0 1)                                 |"
  putStrLn "|  Cond:        (cond [(< x 0) -1] [else 1])                |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "+-- BINDINGS (Variables Locales) ----------------------------+"
  putStrLn "|  Let:         (let ((x 5) (y 3)) (+ x y))                 |"
  putStrLn "|  Let*:        (let* ((x 5) (y (+ x 1))) (+ x y))          |"
  putStrLn "|  LetRec:      (letrec ((f (lambda (n) ...))) (f 10))      |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""
  putStrLn "+-- FUNCIONES -----------------------------------------------+"
  putStrLn "|  Lambda:      (lambda (x y) (+ x y))                      |"
  putStrLn "|  Aplicacion:  ((lambda (x) (* x x)) 5)                    |"
  putStrLn "|  Recursion:   (letrec ((fac (lambda (n) ...))) (fac 5))   |"
  putStrLn "+------------------------------------------------------------+"
  putStrLn ""