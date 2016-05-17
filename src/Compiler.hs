module Compiler where

import Data.List
import Control.Applicative ((<$>),(*>),(<*),(<*>),pure)
import VM 
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Language
import Data.Maybe ( fromJust )
import qualified Text.ParserCombinators.Parsec.Token as P

data Term v =
  TVar v |
  TFun v (Term v) |
  TApply (Term v) (Term v) |
  TNumber Integer |
  TPlus (Term v) (Term v) | TSub (Term v) (Term v) | 
  TMult (Term v) (Term v) | TDiv (Term v) (Term v) |
  TIf (Term v) (Term v) (Term v) |
  TFixfun v v (Term v) |
  TLet v (Term v) (Term v)
  deriving (Show)

type NamedTerm = Term String

lexer = P.makeTokenParser ( emptyDef {
    P.identStart = letter
    , P.identLetter = alphaNum
    , P.reservedNames = ["let"
                            ,"in"
                            ,"fun"
                            ,"fixfun"
                            ,"ifz"
                            ,"then"
                            ,"else" ]
    , P.reservedOpNames = ["=", "+", "-", "*", "->"]
  })

identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer
integer = P.integer lexer

term :: Parser NamedTerm
term = (flip chainl1 term') $
    parens term
  <|>
    try ( TLet <$> (reserved "let" *> identifier <* reserved "=") 
    <*> term
    <*> (reserved "in" *> term))
  <|>
    try( TFun <$> (reserved "fun" *> identifier <* reservedOp "->") <*> term )
  <|>
    try( TFixfun <$> (reserved "fixfun" *> identifier)
    <*> (identifier <* reservedOp "->")
    <*> term )
  <|>
    try (TIf <$> (reserved "ifz" *> term)
        <*> (reserved "then" *> term)
        <*> (reserved "else" *> term))
  <|>
    TNumber <$> integer
  <|>
    TVar <$> identifier

term' :: Parser (NamedTerm -> NamedTerm -> NamedTerm)
term' =
  do
    spaces
    symbol <- optionMaybe (char '+' <|> char '-' <|> char '*' <|> char '/')
    spaces
    return $ case symbol of
      Just '+' -> TPlus
      Just '-' -> TSub
      Just '*' -> TMult
      Just '/' -> TDiv
      Nothing  -> TApply

parsePCF program = parse term "PCF" program

compile :: NamedTerm -> [Instruction]
compile t = go t []
  where
    go (TVar v) e = [Search (fromJust $ elemIndex v e)]
    go (TFun v t) e = [Mkclos (go t (v:e))]
    go (TFixfun v1 v2 t) e = [Mkclos (go t (v2:v1:e))]
    go (TApply t1 t2) e = Pushenv : (go t2 e) ++ [Push] ++ (go t1 e) ++ [Apply, Popenv]
    go (TNumber n) e = [Ldi n]
    go (TPlus t1 t2) e = (go t2 e) ++ [Push] ++ (go t1 e) ++ [Add]
    go (TSub t1 t2) e = (go t2 e) ++ [Push] ++ (go t1 e) ++ [Sub]
    go (TMult t1 t2) e = (go t2 e) ++ [Push] ++ (go t1 e) ++ [Mult]
    go (TDiv t1 t2) e = (go t2 e) ++ [Push] ++ (go t1 e) ++ [Div]
    go (TIf t1 t2 t3) e = (go t1 e) ++ [Test (go t2 e) (go t3 e)]
    go (TLet v t1 t2) e = Pushenv : (go t1 e) ++ [Extend] ++ (go t2 (v:e)) ++ [Popenv]

compileProgram :: String -> Maybe [Instruction]
compileProgram program = case parsePCF program of
  Left _ -> Nothing
  Right ast -> Just $ compile $ ast
