--http://jakewheat.github.io/intro_to_parsing/

--cabal update
--cabal sandbox init
--cabal install parsec
--cabal repl

import Text.Parsec.Char (anyChar)
import Text.Parsec.String (Parser)


--anyChar :: Parser Char


type CoeficientType = Int
type SymbolType = Char
type OperationType = Char


data AbstractEquation a b c = Coeficient a | Symbol b | Null |
                              Combination (AbstractEquation a b c)
                                          c
                                          (AbstractEquation a b c)
                                 deriving (Show)

type Equation = AbstractEquation CoeficientType SymbolType OperationType


--AbstractEquation
