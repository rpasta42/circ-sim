
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
