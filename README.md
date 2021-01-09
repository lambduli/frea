# Frea

A simple programming language with Damas-Hindley-Milner type inference.

To compiler: `$ stack build`

To run: `$ stack run`

## Examples:
### Maping over a list
```
let
  list = [1, 2, 3, 4, 5, 6, 7]
in let
  double = (\ x -> (#* (2,x)))
in let
  map = fix (\ map lst fn ->
              if (#nil? lst)
              then []
              else (#: ((fn (#head lst)), (map (#tail lst) fn))))
in (map list double)
```
### Factorial of 5
```
let
  zero = (\ n -> (#= (n,0)))
in let
  fact = fix (\ fact n ->
                  if (zero n)
                  then 1
                  else (#* (n, (fact (#- (n, 1))))))
in (fact 5)
```

___

> In the REPL, you have to hit enter two times, that is the consequence of the very naive but simple implementation of multine line expressions, which are very convenient and may actually pay for the annoyance of double enter.

## REPL commands:
- `:t` *followed by an expression* does not evaluate the expression but rather tells you it's type
- *expression* standing on it's own will be typechecked and possibly evaluated (it can span across multiple lines)

Language supports:

## Various Simple Literals

```
frea λ> :t 23

:: Int
```
```
frea λ> :t 23.23

:: Double
```
```
frea λ> :t #t

:: Bool

frea λ> :t #f

:: Bool
```
```
frea λ> :t 'c'

:: Char
```
```
frea λ> :t "hello world"

:: String
```

## Some More Interesting Ones
### Lists
```
frea λ> :t [1, 2, 3]

:: [Int]
```
### Tuples of arbitrary length
```
frea λ> :t (1, "string", 'c')

:: (Int, String, Char)
```
### Unit
```
frea λ> :t ()

:: Unit
```
### Functions of course
```
frea λ> :t (\ int -> (#+ (int, 1)) )

:: Int -> Int
```
_____

## Expressions:

### Conditionals
```
frea λ> if #t then 23 else 42

23
```

### Let in expression
```
frea λ> let name = "Frea" in (#. (name, " is awesome!"))

"Frea is awesome!"
```

### Function Application
```
frea λ> (fn arg1 arg2 arg3 ... argN)
```

### Lambdas with many arguments sugar
```
frea λ> (\ a b c -> c)
```

### Recursion using Fix keyword
```
frea λ> (fix (\ fact n -> if (#= (n,0)) then 1 else (#* (n,(fact (#- (n,1)))))) 5)

120
```
___

> Binary primitive operations take tuple with two values!

### Supported built-in operations:
- `#=` :: forall a . (a, a) -> Bool
- `#<` :: (Int, Int) -> Bool
- `#>` :: (Int, Int) -> Bool
- `#+` :: (Int, Int) -> Bool
- `#*` :: (Int, Int) -> Bool
- `#-` :: (Int, Int) -> Bool
- `#/` :: (Int, Int) -> Bool
- `#.` :: (String, String) -> String
- `#++` :: forall a . ([a], [a]) -> [a]
- `#;` :: (Char, String) -> String
- `#:` :: forall a . (a, [a]) -> [a]
- `#!!` :: forall a . (Int, [a]) -> a
- `#head` :: forall a . [a] -> a
- `#tail` :: forall a . [a] -> [a]
- `#nil?` :: forall a . [a] -> Bool
- `#fst` :: forall a b . (a, b) -> a
- `#snd` :: forall a b . (a, b) -> b
