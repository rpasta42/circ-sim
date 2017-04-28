---sudo apt-get install freeglut3 freeglut3-dev
---sudo apt install cabal; cabal update; cabal install glut
--mkdir /tmp/notes_serv; sshfs root@69.164.203.126:/ /tmp/notes_serv

import Graphics.UI.GLUT

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $
   mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

