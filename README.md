# pcf-compiler

Compiler for PCF (Programming Language for Computable Functions) written in Haskell

```
$ stack build
$ echo 'let inc = fun x -> x + 1 in inc 5' | stack runghc app/Main.hs
Inst: [Pushenv,Mkclos [Ldi 1,Push,Search 0,Add],Extend,Pushenv,Ldi 5,Push,Search 0,Apply,Popenv,Popenv]
Result: Right (Nat 6)
```
