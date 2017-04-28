---sudo apt-get install freeglut3 freeglut3-dev
---sudo apt install cabal; cabal update; cabal install glut
--mkdir /tmp/notes_serv; sshfs root@69.164.203.126:/ /tmp/notes_serv

import Graphics.UI.GLUT

type GlVert = [(GLfloat, GLfloat, GLfloat)]

getCirclePoints :: Float -> GlVert
getCirclePoints n = [ (sin (2*pi*k/n), cos (2*pi*k/n), 0) | k <- [1..n] ]

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = getCirclePoints 100


-- renderMode (renderPrimitive) types
--Points/Triangles/TriangleStrip/TriangleFan/Lines/
--LineLoop/LineStrip/Quads/QuadStrip/Polygon
drawVertices :: (Foldable t1, VertexComponent a) => t -> t1 (a, a, a) -> IO ()
drawVertices renderMode vertices = renderPrimitive Polygon $
   mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) vertices


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop


display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  drawVertices Polygon myPoints
  flush

