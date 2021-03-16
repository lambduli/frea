# Frea

A simple programming language with Damas-Hindley-Milner type inference.

To compile: `$ stack build`

To run: `$ stack run`

To test: `$ stack test`

## Examples:
### Maping over a list
```haskell
let
  list = [1, 2, 3, 4, 5]
  double x = (2 * x)
  map lst fn = if (nil? lst)
                then []
                else ((fn (head lst)) : (map (tail lst) fn))
in (map list double)
```
### Factorial of 5
```haskell
let
  zero n = (n == 0)
  dec n = (n - 1)
  fact n = if (zero n)
            then 1
            else (n * (fact (dec n)))
in (fact 5)
```

___

> In the REPL, you have to hit enter two times, that is the consequence of the very naive but simple implementation of multine line expressions, which are very convenient and may actually pay for the annoyance of double enter.

## REPL commands:
- `:t` *followed by an expression* does not evaluate the expression but rather tells you it's type
- `:exit` or `:q` exits the REPL
- *expression* standing on it's own will be typechecked and possibly evaluated (it can span across multiple lines)

____
Language supports:

## Various Simple Literals

```haskell
frea λ > :t -23

:: Int
```
```haskell
frea λ > :t 23.23

:: Double
```
```haskell
frea λ > :t #t

:: Bool

frea λ > :t #f

:: Bool
```
```haskell
frea λ > :t 'c'

:: Char
```
```haskell
frea λ > :t "hello world"

:: String
```

## Some More Interesting Ones
### Lists
```haskell
frea λ > :t [1, 2, 3]

:: [Int]
```
### Tuples of arbitrary size
```haskell
frea λ > :t (1, "string", 'c')

:: (Int, String, Char)
```
### Unit
```haskell
frea λ > :t ()

:: Unit
```
### Functions of course
```haskell
frea λ > :t (\ int -> (int + 1))

:: Int -> Int
```
### Operators
```haskell
frea λ > :t (+)

:: Int -> Int -> Int
```
> The built-in "binary" operators (those starting with #) accept a single tuple and can not be used in infix. Therefore they must be wrapped in parenteses to work in the prefix notation.

> Other (binary) operators can be used in infix as well as (binary) functions with help of backtick symbol like in the Haskell.

```haskell
frea λ > let plus a b = (a + b) in (23 `plus` 42)

65
```
_____

## Expressions:

> Operator names can start with these symbols `!` `$` `#` `%` `&` `*` `+` `.` `/` `<` `=` `>` `?` `@` `\` `^` `|` `-` `~` `:` `;`, they can also contain ordinary alphabetical characters.

> Variable names can start with lower and upper case letters and can contain those and symbols and numbers.

### Conditionals
```haskell
frea λ > if #t then 23 else 42

23
```

### Let in expression
```haskell
frea λ > let name = "Frea" in (name . " is awesome!")

"Frea is awesome!"
```

You can also define binary operators or functions using infix notation:

```haskell
frea λ > let a `plus` b = (a + b) in (23 `plus` 42)

65
```

or operator:

```haskell
frea λ > let a |> b = b in ("left" |> "right")

"right"
```

### Operators
```haskell
frea λ > ((+) 23 42)

```

or

```haskell
frea λ > (23 + 42)

65
```

### Function Application
```haskell
frea λ > (fn arg1 arg2 arg3 ... argN)
```

or

```haskell
frea λ> (arg1 `fn` arg2)
```

### Lambdas with many arguments
```haskell
frea λ > (\ a b c -> c)
```

> You can use two different keywords for lambdas: `lambda` and `\`. When you use `\` you need to always put a space behind it, separating the first argument and the `\` symbol. That's because you can use any symbol to name operators, even `\`. Therefore `\i` is a valid variable name in Frea.

### Recursion using Fix keyword
```haskell
frea λ > (fix (\ fact n -> if (n == 0) then 1 else (n * (fact (n - 1)))) 5)

120
```

### Recursion using Let rec
```haskell
let
  zero n = (n == 0)
  dec n = (n - 1)
in let rec
  fact n = if (zero n)
            then 1
            else (n * (fact (dec n)))
in (fact 5)
```

> This is archaic, more verbose version. You can safely ommit the `rec` keyword.

___

### Primitive operations

> "Binary" primitive operations take tuple with two values!

Those operators are the low level machinery which is used to implement the small prelude.
You you can use them to implement your own functions and operators.

## Supported built-in operations:
- `(#=)` :: forall a . (a, a) -> Bool
- `(#<)` :: forall a . (a, a) -> Bool
- `(#>)` :: forall a . (a, a) -> Bool
- `(#&&)` :: (Bool, Bool) -> Bool
- `(#||)` :: (Bool, Bool) -> Bool
- `(#+)` :: (Int, Int) -> Int
- `(#+.)` :: (Double, Double) -> Double
- `(#*)` :: (Int, Int) -> Int
- `(#*.)` :: (Double, Double) -> Double
- `(#-)` :: (Int, Int) -> Int
- `(#-.)` :: (Double, Double) -> Double
- `(#div)` :: (Int, Int) -> Int
- `(#/)` :: (Double, Double) -> Double
- `(#++)` :: forall a . ([a], [a]) -> [a]
- `(#:)` :: forall a . (a, [a]) -> [a]
- `(#head)` :: forall a . [a] -> a
- `(#tail)` :: forall a . [a] -> [a]
- `(#nil?)` :: forall a . [a] -> Bool
- `(#fst)` :: forall a b . (a, b) -> a
- `(#snd)` :: forall a b . (a, b) -> b

### Declaring bindings in the REPL

You can just write a list of bindings as in `let in` expression but dropping the `let` and `in` keywords.
You can write stuff like:

```haskell
a == b  = ((#=) (a, b))
a < b   = ((#<) (a, b))
a > b   = ((#>) (a, b))

a + b   = ((#+) (a, b))
a +. b  = ((#+.) (a, b))
```

### Evaluation Strategy

Frea employs `"call-by-name"` semantics. Programs like this one are absolutely valid:

```haskell
forever n = (forever n)

result = (forever 0)

lst = (result : [1, 2, 3])

(lst !! 1)
```
And won't trigger infinite loop.
