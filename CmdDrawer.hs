module CmdDrawer (
  drawCircuit
) where


---sudo apt-get install freeglut3 freeglut3-dev
---sudo apt install cabal; cabal update; cabal install glut
--mkdir /tmp/notes_serv; sshfs root@69.164.203.126:/ /tmp/notes_serv


import Circuit


drawCircuit :: Circuit a b -> String
drawCircuit (Circuit elements) = drawCircuit' elements

drawCircuit' :: [(CircuitElement a, DrawData b)]

drawCircuit' [] = ""
drawCircuit' (x:[]) =
