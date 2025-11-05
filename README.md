# MiniLisp - Intérprete Funcional

Implementación completa de MiniLisp en Haskell para el curso de
Lenguajes de Programación, UNAM Facultad de Ciencias.

## Características

- ✅ Sintaxis estilo Scheme/Lisp
- ✅ Operadores variádicos (+, -, *, /, =, <, >, etc.)
- ✅ Estructuras de datos: pares y listas
- ✅ Funciones de orden superior (map, filter)
- ✅ Recursión con letrec
- ✅ Condicionales: if, if0, cond
- ✅ Bindings: let, let*, letrec
- ✅ Funciones currificadas automáticamente
- ✅ REPL interactivo
- ✅ Carga de archivos

## Instalación

### Requisitos

- GHC >= 9.2
- Cabal >= 3.6

### Pasos
```bash
# Clonar repositorio
git clone https://github.com/GiovanniAE/MiniLisp.git
cd minilisp

# Compilar proyecto
cabal build

# Ejecutar REPL
cabal run minilisp

# Ejecutar archivo
cabal run minilisp examples/factorial.minisp
```

## Uso

### REPL Interactivo
```bash
cabal run minilisp
```

Comandos disponibles:
- `:salir` - Salir del REPL
- `:ayuda` - Mostrar ayuda
- `:cargar <archivo>` - Cargar archivo
- `:nucleo <expresión>` - Mostrar núcleo desazucarizado

### Ejecutar archivos
```bash
cabal run minilisp archivo.minisp
```

## Ejemplos

Los archivos de ejemplo incluyen múltiples casos de prueba:

### 📊 Factorial ([`factorial.minisp`](examples/factorial.minisp))
```bash
cabal run minilisp examples/factorial.minisp
# Resultados: 1, 6, 120, 5040, 3628800
```

### ➕ Suma 1..n ([`sum.minisp`](examples/sum.minisp))
```bash
cabal run minilisp examples/sum.minisp
# Resultados: 15, 55, 210, 5050
```

### 🌀 Fibonacci ([`fibonacci.minisp`](examples/fibonacci.minisp))
```bash
cabal run minilisp examples/fibonacci.minisp
# Resultados: 0, 1, 5, 21, 55
```

### ⚡ Potencia ([`power.minisp`](examples/power.minisp))
```bash
cabal run minilisp examples/power.minisp
# Resultados: 8, 81, 25, 1000
```

### 🔢 MCD - Máximo Común Divisor ([`mcd.minisp`](examples/mcd.minisp))
```bash
cabal run minilisp examples/mcd.minisp
# Resultados: 6, 5, 6, 1
```

Cada archivo contiene varios casos de prueba con diferentes parámetros para demostrar la función con múltiples entradas.

## Arquitectura

- [`src/SurfaceAST.hs`](src/SurfaceAST.hs) - AST de sintaxis superficial
- [`src/Parser.hs`](src/Parser.hs) - Parser con Megaparsec
- [`src/AST.hs`](src/AST.hs) - AST del núcleo
- [`src/Desugar.hs`](src/Desugar.hs) - Desazucarización
- [`src/Eval.hs`](src/Eval.hs) - Evaluador
- [`src/Main.hs`](src/Main.hs) - REPL y CLI