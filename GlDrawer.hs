---sudo apt-get install freeglut3 freeglut3-dev
---sudo apt install cabal; cabal update; cabal install glut
--mkdir /tmp/notes_serv; sshfs root@69.164.203.126:/ /tmp/notes_serv

import Graphics.UI.GLUT
import Types
import Circuit

type GlVerts = [(GLfloat, GLfloat, GLfloat)]



data Shape a = ShapeSquare { width :: a }
             | ShapeRect { width :: a
                         , height :: a
                         }
             | ShapeCircle { radius :: a
                           , numVerts :: a
                           }
             | ShapeLine { start :: a
                         , end :: a
                         }


data DrawObject a t = DrawObject { shape :: (Shape a)
                                 , position :: Point a
                                 , fillType :: t
                                 , color :: String
                                 , scale :: a
                                 , label :: String
                                 }


--a = unit for electric data, b = unit for location
--c = numeric unit for OpenGL, d = renderMode type
drawCircuit :: Circuit a b -> [DrawObject c d]
drawCircuit x = []



getCirclePoints :: Float -> GlVerts
getCirclePoints n = [ (sin (2*pi*k/n), cos (2*pi*k/n), 0) | k <- [1..n] ]

myPoints :: GlVerts
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

   --https://hackage.haskell.org/package/GLUT-2.7.0.12/docs/Graphics-UI-GLUT-Callbacks-Window.html#g:9
   --mouseCallback $= onMouse
   motionCallback $= Just onMouseMove

   mainLoop


display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  drawVertices Polygon myPoints
  flush


onMouseMove :: MotionCallback
onMouseMove position = print (show position)

