
type CoeficientType = Int
type SymbolType = Char
type OperationType = Char


data AbstractEquation a b c = Coeficient a | Symbol b | Operation c | Null |
                              Combination (AbstractEquation a b c)
                                          (AbstractEquation a b c)
                                          (AbstractEquation a b c)
                                 deriving (Show)

type Equation = AbstractEquation CoeficientType SymbolType OperationType

mkEq :: String -> Equation
mkEq [] = Null
mkEq (' ':xs) = mkEq xs
mkEq ('*':xs) = Coeficient '*'
--mkEq
