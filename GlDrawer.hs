---sudo apt-get install freeglut3 freeglut3-dev
---sudo apt install cabal; cabal update; cabal install glut
--mkdir /tmp/notes_serv; sshfs root@69.164.203.126:/ /tmp/notes_serv
import Graphics.UI.GLUT

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

