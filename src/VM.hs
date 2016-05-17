module VM where

data Instruction =
  Ldi Integer |
  Push |
  Extend |
  Search Int |
  Pushenv |
  Popenv |
  Mkclos [Instruction] |
  Apply |
  Test [Instruction] [Instruction] |
  Add |
  Sub |
  Mult |
  Div deriving (Show)

data Value = Nat Integer | Closure [Instruction] Env deriving (Show)
type Env   = [Value]
data SElem = SVal Value | SEnv Env deriving (Show)
type State = (Value, [SElem], Env, [Instruction])

buildState :: [Instruction] -> State
buildState inst = (Nat 0, [], [], inst)

run :: State -> Either State Value
run (a,s,e,[]) = Right a
run (a,s,e,(Mkclos i):c) = run (Closure i e, s, e, c)
run (a,s,e,Push:c) = run (a, (SVal a):s, e, c)
run (a,s,e,Extend:c) = run (a, s, e ++ [a], c)
run (a,s,e,(Search n):c) = run (v, s, e, c)
  where
    v = (reverse e) !! n
run (a,s,e,Pushenv:c) = run (a, (SEnv e):s, e, c)
run (a,(SEnv e'):s,e,Popenv:c) = run (a,s,e',c)
run (Closure i e', (SVal w):s, e, Apply:c) = run (Closure i e', s, e' ++ [Closure i e', w], i ++ c)
run (a,s,e,(Ldi n):c) = run (Nat n, s, e, c)
run (Nat n,(SVal (Nat m)):s, e, Add:c) = run (Nat $ n + m, s, e, c)
run (Nat n,(SVal (Nat m)):s, e, Sub:c) = run (Nat $ n - m, s, e, c)
run (Nat n,(SVal (Nat m)):s, e, Mult:c) = run (Nat $ n * m, s, e, c)
run (Nat n,(SVal (Nat m)):s, e, Div:c) = run (Nat $ n `div` m, s, e, c)
run (Nat 0, s, e,(Test i j):c) = run (Nat 0, s, e, i ++ c)
run (Nat n, s, e,(Test i j):c) = run (Nat n, s, e, j ++ c)
run (a,s,e,c) = Left (a, s, e, c)
