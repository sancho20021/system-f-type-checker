# System-f type-checker
System-F type-checker written in Haskell

## Usage
```input ::= [context '|-'] expr [':' type]```

- Omit context if you want to input expression without free variables
- Omit type if you want it to be inferred


## Run instructions
0. Make sure you have [cabal-install](https://cabal.readthedocs.io/en/latest/developing-packages.html) installed
1. Go to [./system-f-typechecker/](https://github.com/sancho20021/system-f-type-checker/tree/main/system-f-typechecker) directory
2. In the command line type the desired instruction:
    - ```make run``` - run the typecheker
    - ```make runShow``` - run the typecheker with showing detailed errors
    - ```make test``` - run tests for the typechecker
    - ```make clean``` - clean generated build directory

## Lambda expression examples
- ```/\ a. /\b. \x : a. \y : b. x : forall a. forall b. a -> b -> a```
- ```x : forall a.a |- x : forall a.a```
- ```\b : Y -> Y. \a : Y. b a: (Y -> Y) -> Y -> Y```
- ```(/\Y. \c : Y. c)[Int]: Int -> Int```

## Grammar
    input            ::= [context '|-'] expr [':' type]

    context          ::= eps | variable ':' type [',' context]

    type             ::= variable
                    | (type)
                    | (type) '->' type
                    | variable '->' type
                    | forall variable. type

    expr             ::= [application] '\' variable ':' type '.' expr
                    | [application] '/\' variable '.' expr
                    | application

    application      ::= type_application
                    | application type_application

    type_application ::= atom
                    | type_application '[' type ']'

    atom             ::= (expr)
                    | variable

    variable         ::= [a-z] [a-z0-9'_]*
