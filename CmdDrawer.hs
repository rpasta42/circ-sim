module CmdDrawer (
  drawCircuit
) where




import Circuit


drawCircuit :: Circuit a b -> String
drawCircuit (Circuit elements) = drawCircuit' elements

drawCircuit' :: [(CircuitElement a, DrawData b)]

drawCircuit' [] = ""
drawCircuit' (x:[]) = ""
