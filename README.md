# Frea

A simple programming language with Damas-Hindley-Milner type inference.

To compile: `$ stack build`

To run: `$ stack run`

To test: `$ stack test`

## Examples:
### Maping over a list
```haskell
let
  { list = [1, 2, 3, 4, 5]
  ; double x = 2 * x
  ; map fn lst = which-List lst
                  []
                  \ h t -> (fn h) : (map fn t) }
in map double list
```
### Computing Factorial
```haskell
let
  { zero n = n == 0
  ; dec n = n - 1
  ; fact n =  if zero n
              then 1
              else n * (fact (dec n)) }
in fact 5
```

___

> In the REPL, you have to hit enter two times, that is the consequence of the very naive but simple implementation of multine line expressions, which are very convenient and may actually pay for the annoyance of double enter.

## REPL commands:
- `:t` *followed by an expression* does not evaluate the expression but rather tells you it's type
- `:k` *followed by a type* checks what `kind` does the `type` have
- `:exit` or `:q` / `:Q` exits the REPL
- *expression* standing on it's own will be typechecked and possibly evaluated (it can span across multiple lines)

____
Language supports:

## Various Simple Literals

```haskell
frea λ > :t -23

-23 :: Int
```
```haskell
frea λ > :t 23.23

23.23 :: Double
```
```haskell
frea λ > :t True

True :: Bool

frea λ > :t False

False :: Bool
```
```haskell
frea λ > :t 'c'

'c' :: Char
```
```haskell
frea λ > :t "hello world"

"hello world" :: List Char
```

## Some More Interesting Ones
### Lists
```haskell
frea λ > :t [1, 2, 3]

[1, 2, 3] :: [Int]
```
### Tuples of arbitrary size
```haskell
frea λ > :t (1, "string", 'c')

(1, "string", 'c') :: (Int, String, Char)
```
### Unit
```haskell
frea λ > :t ()

() :: ()
```
### Functions of course
```haskell
frea λ > :t (\ int -> (int + 1))

(\ int -> ((+ int) 1)) :: Int -> Int
```
### Operators
```haskell
frea λ > :t (+)

(+) :: Int -> Int -> Int
```

> Operators can be used in infix as well as functions wrapped in the pair of backtics.

```haskell
frea λ > let { a `plus` b = a + b } in 23 `plus` 42

65
```
_____

## Custom Data Types

You can use a keyword `data` to define you own data types.

For example this is how the `Bool` type is defined in the `prelude.frea`:

```haskell
data Bool
  = True
  | False
```

or a polymorphic linked list:

```haskell
data List a
  = []
  | a : (List a)
```

> The type parameters in the data declaration must be a lower-case-starting identifiers - true variables. 

## Elimination of the Custom Data Types

Opposite to the constructor, which constructs new value of your data type, there is a `which-_` eliminator for each defined data type.

Specific eliminator will be named after this scheme: starting with `which-` and following the name of the type you want to eliminate.

For example: `which-Bool` for `Bool` and `which-List` for `List`.

Eliminator is special function, which takes a value of the type you wish to eliminate folowed by `N` values - each of these is a counterpart to one specific constructor of your data type and it will be evaluated if and only if the actual value was constructed by that constructor.

Which value corresponds to which constructor is determined by the order.

It works like this:

```haskell
module Maybe where
{ data Maybe a = Nothing | Just a

; data List a
    = []
    | a : (List a)

; head lst =  which-List
                lst
                Nothing
                \ head tail -> Just head
}
```

Thanks to the evaluation strategy of the Frea you don't need to worry about evaluation of the expressions, which won't be selected, they will never be evaluated.

_____

## Expressions:

> Operator names can start with these symbols `!` `$` `#` `%` `&` `*` `+` `.` `/` `<` `=` `>` `?` `@` `\` `^` `|` `-` `~` `;`, they can also contain ordinary alphabetical characters.

> Constructor operator's names start with `:` and can contain other symbols and alphabetical characters.

> Variable names must start with lower case letter and can contain those, upper case letters and symbols and numbers.

> Constructor names must start with upper case letter and can contain those, lower case letters and symbols and numbers.

### Conditionals
```haskell
frea λ > if True then 23 else 42

23
```

### Let in expression
```haskell
frea λ > let { name = "Frea" } in name  ++ " is awesome!"

"Frea is awesome!"
```

You can also define binary operators or functions using infix notation:

```haskell
frea λ > let { a `plus` b = a + b } in 23 `plus` 42

65
```

or operator:

```haskell
frea λ > let { a |> b = b } in "left" |> "right"

"right"
```

### Operators
```haskell
frea λ > (+) 23 42

```

or

```haskell
frea λ > 23 + 42

65
```

### Function Application
```haskell
frea λ > fn arg1 arg2 arg3 ... argN
```

or

```haskell
frea λ> arg1 `fn` arg2
```

### Lambdas with many arguments
```haskell
frea λ > \ a b c -> c
```

> You can use two different keywords for lambdas: `lambda` and `\`. When you use `\` you need to always put a space behind it, separating the first argument and the `\` symbol. That's because you can use any symbol to name operators, even `\`. Therefore `\i` is a valid variable name in Frea.

<!--
### Recursion using Fix keyword
```haskell
frea λ > (fix (\ fact n -> if (n == 0) then 1 else (n * (fact (n - 1)))) 5)

120
```

### Recursion using Let rec
```haskell
let
  { zero n = n == 0
  ; dec n = n - 1
  ; fact n =  if zero n
              then 1
              else n * (fact (dec n))
  } in fact 5
```
-->

___

### Primitive operations

> "Binary" primitive operations take tuple with two values!

Those operators are the low level machinery which is used to implement the small prelude.
You you can use them to implement your own functions and operators.

## Supported built-in operations:
- `(#=)` :: forall a . (a, a) -> Bool
- `(#<)` :: forall a . (a, a) -> Bool
- `(#>)` :: forall a . (a, a) -> Bool
- `(#+)` :: (Int, Int) -> Int
- `(#+.)` :: (Double, Double) -> Double
- `(#*)` :: (Int, Int) -> Int
- `(#*.)` :: (Double, Double) -> Double
- `(#-)` :: (Int, Int) -> Int
- `(#-.)` :: (Double, Double) -> Double
- `(#div)` :: (Int, Int) -> Int
- `(#/)` :: (Double, Double) -> Double
- `(#fst)` :: forall a b . (a, b) -> a
- `(#snd)` :: forall a b . (a, b) -> b
- `(#show)` :: forall a . a -> List Char

### Declaring bindings in the REPL

You can just write a list of bindings as in `let in` expression but dropping the `let` and `in` keywords.
You can write stuff like:

```haskell
module Module where
{ a == b  = (#=) (a, b)
; a < b   = (#<) (a, b)
; a > b   = (#>) (a, b)
; a + b   = (#+) (a, b)
; a +. b  = (#+.) (a, b)
}
```

> For various reasons if you want to submit new global bindings you must write it like a module definition. The name of the module does not matter currently.

### Typ Annotations

You can optionally add type annotations to your top level declarations.

```haskell
module Module where
{ foo :: a -> Bool
; foo n = True
}
```

You can also annotate expressions.

```haskell
let { id = (\ i -> i) } in [((id 42) :: Int), id 23]
```

> As of now, type annotations inside the expression needs to be wrapped in set of parentheses.

> Frea is still able to infer the principle type without any type annotation.


### Evaluation Strategy

Frea employs `"lazy"` evaluation. Programs like this one are absolutely valid:

```haskell
{ forever n = forever n
; result = forever 0
; lst = result : [1, 2, 3] }

lst !! 1
```
And won't trigger infinite loop.

Nothing should get executed more than once, so program like this one:
```haskell
{ num = #debug 23
; fun a b = a + b }

fun num num
```

should produce only one
```
@debug  `23`
```
not two, even thought `num` is refered to two times.
