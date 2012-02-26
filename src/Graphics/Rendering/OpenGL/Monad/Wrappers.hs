-- | Defines simple wrappers around any OpenGL functions in use, letting us
--   use them inside the GL monad. Horray for tedious lifting. I'm basically
--   doing all of this on demand, so feel free to add your own wrappers as you
--   need them. This is much to big of a task to do all at once.
module Graphics.Rendering.OpenGL.Monad.Wrappers ( module Graphics.Rendering.OpenGL.Raw.Core31
                                                , OGL.Vertex2(..)
                                                , OGL.TexCoord2(..)
                                                , OGL.Color
                                                , OGL.Color3(..)
                                                , OGL.Color4(..)
                                                , OGL.PrimitiveMode(..)
                                                , OGL.TextureCoordName(..)
                                                , OGL.Clamping(..)
                                                , OGL.Repetition(..)
                                                , OGL.TextureFilter(..)
                                                , OGL.TextureFunction(..)
                                                , OGL.Capability(..)
                                                , OGL.TextureObject(..)
                                                , OGL.TextureTarget(..)
                                                , OGL.ClearBuffer(..)
                                                , OGL.MatrixMode(..)
                                                , OGL.Vector3(..)
                                                , OGL.MatrixComponent()
                                                , OGL.ObjectName()
                                                , OGL.CubeMapTarget(..)
                                                , OGL.Proxy(..)
                                                , OGL.Level()
                                                , OGL.PixelInternalFormat(..)
                                                , OGL.TextureSize2D(..)
                                                , OGL.Border()
                                                , OGL.PixelData(..)
                                                , OGL.PixelFormat(..)
                                                , OGL.DataType(..)
                                                , OGL.Size(..)
                                                , OGL.BlendingFactor(..)
                                                , OGL.HintMode(..)
                                                , OGL.HintTarget(..)
                                                , GLUT.DisplayMode(..)
                                                , OGL.textureWrapMode
                                                , OGL.textureFilter
                                                , OGL.texture
                                                , OGL.textureFunction
                                                , OGL.textureBinding
                                                , OGL.matrixMode
                                                , OGL.clearColor
                                                , OGL.lineSmooth
                                                , OGL.pointSmooth
                                                , OGL.polygonSmooth
                                                , OGL.blend
                                                , OGL.blendFunc
                                                , OGL.lineWidth
                                                , OGL.hint
                                                , OGL.viewport
                                                , OGL.currentColor
                                                , GLUT.initialDisplayMode
                                                , GLUT.displayCallback
                                                , GLUT.currentWindow
                                                , GLUT.screenSize
                                                , GLUT.windowSize
                                                , GLUT.reshapeCallback
                                                , GLUT.keyboardMouseCallback
                                                , GLUT.motionCallback
                                                , GLUT.fullScreenMode
                                                , vertex
                                                , texCoord
                                                , vertex', texCoord' -- CUSTOM
                                                , renderPrimitive
                                                , ($=)
                                                , color
                                                , clear
                                                , loadIdentity
                                                , swapBuffers
                                                , preservingMatrix
                                                , translate
                                                , rotate
                                                , scale
                                                , genObjectNames
                                                , deleteObjectNames
                                                , isObjectName
                                                , texImage2D
                                                , getArgsAndInitialize
                                                , get
                                                , ortho2D
                                                -- * Logging Functions
                                                , glDebugM
                                                , glInfoM
                                                , glNoticeM
                                                , glWarningM
                                                , glErrorM
                                                , glCriticalM
                                                , glAlertM
                                                , glEmergencyM
                                                ) where

import qualified Graphics.Rendering.OpenGL.GL as OGL
import qualified Graphics.Rendering.OpenGL.GLU as OGLU
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Monad.Unsafe
import Graphics.Rendering.OpenGL.Raw.Core31 ( GLbitfield
                                            , GLboolean
                                            , GLbyte
                                            , GLchar
                                            , GLclampd
                                            , GLclampf
                                            , GLdouble
                                            , GLenum
                                            , GLfloat
                                            , GLhalf
                                            , GLint
                                            , GLintptr
                                            , GLshort
                                            , GLsizei
                                            , GLsizeiptr
                                            , GLubyte
                                            , GLuint
                                            , GLushort
                                            )
import System.Log.Logger

vertex :: OGL.Vertex a => a -> GL ()
vertex = unsafeRunOnGraphicsCard . OGL.vertex
{-# INLINE vertex #-}

texCoord :: OGL.TexCoord a => a -> GL ()
texCoord = unsafeRunOnGraphicsCard . OGL.texCoord
{-# INLINE texCoord #-}

-- | Sends the draw command for one vertex. Meant to be used in renderPrimitive
--   and its ilk.
vertex' :: Int -> Int -> GL ()
vertex' x y = vertex $ (OGL.Vertex2 :: GLdouble
                                    -> GLdouble
                                    -> OGL.Vertex2 GLdouble)
                          (fromIntegral x)
                          (fromIntegral y)
{-# INLINE vertex' #-}

-- | Just like 'vertex', except for a texture coordinate.
--
--   See: 'vertex'
texCoord' :: Int -> Int -> GL ()
texCoord' x y = texCoord $ (OGL.TexCoord2 :: GLdouble
                                         -> GLdouble
                                         -> OGL.TexCoord2 GLdouble)
                              (fromIntegral x)
                              (fromIntegral y)
{-# INLINE texCoord' #-}

renderPrimitive :: OGL.PrimitiveMode -> GL () -> GL ()
renderPrimitive pm = unsafeRunOnGraphicsCard . OGL.renderPrimitive pm . runGraphics
{-# INLINE renderPrimitive #-}

($=) :: OGL.HasSetter s => s a -> a -> GL ()
($=) s v = unsafeRunOnGraphicsCard $ s OGL.$= v
{-# INLINE ($=) #-}

color :: OGL.Color a => a -> GL ()
color = unsafeRunOnGraphicsCard . OGL.color
{-# INLINE color #-}

clear :: [OGL.ClearBuffer] -> GL ()
clear = unsafeRunOnGraphicsCard . OGL.clear
{-# INLINE clear #-}

loadIdentity :: GL ()
loadIdentity = unsafeRunOnGraphicsCard OGL.loadIdentity
{-# INLINE loadIdentity #-}

swapBuffers :: GL ()
swapBuffers = unsafeRunOnGraphicsCard GLUT.swapBuffers
{-# INLINE swapBuffers #-}

preservingMatrix :: GL () -> GL ()
preservingMatrix = unsafeRunOnGraphicsCard . OGL.preservingMatrix . runGraphics
{-# INLINE preservingMatrix #-}

translate :: OGL.MatrixComponent c => OGL.Vector3 c -> GL ()
translate = unsafeRunOnGraphicsCard . OGL.translate
{-# INLINE translate #-}

rotate :: OGL.MatrixComponent c => c -> OGL.Vector3 c -> GL ()
rotate x = unsafeRunOnGraphicsCard . OGL.rotate x
{-# INLINE rotate #-}

scale :: OGL.MatrixComponent c => c -> c -> c -> GL ()
scale a b = unsafeRunOnGraphicsCard . OGL.scale a b
{-# INLINE scale #-}

genObjectNames :: OGL.ObjectName a => Int -> GL [a]
genObjectNames = unsafeRunOnGraphicsCard . OGL.genObjectNames
{-# INLINE genObjectNames #-}

deleteObjectNames :: OGL.ObjectName a => [a] -> GL ()
deleteObjectNames = unsafeRunOnGraphicsCard . OGL.deleteObjectNames
{-# INLINE deleteObjectNames #-}

isObjectName :: OGL.ObjectName a => a -> GL Bool
isObjectName = unsafeRunOnGraphicsCard . OGL.isObjectName
{-# INLINE isObjectName #-}

texImage2D :: Maybe OGL.CubeMapTarget -> OGL.Proxy -> OGL.Level -> OGL.PixelInternalFormat -> OGL.TextureSize2D -> OGL.Border -> OGL.PixelData a -> GL ()
texImage2D a b c d e f = unsafeRunOnGraphicsCard . OGL.texImage2D a b c d e f
{-# INLINE texImage2D #-}

getArgsAndInitialize :: GL (String, [String])
getArgsAndInitialize = unsafeRunOnGraphicsCard GLUT.getArgsAndInitialize
{-# INLINE getArgsAndInitialize #-}

-- | Gets a 'StateVar'.
get :: OGL.HasGetter g => g a -> GL a
get = unsafeRunOnGraphicsCard . OGL.get
{-# INLINE get #-}

ortho2D :: Double -> Double -> Double -> Double -> GL ()
ortho2D a b c d = unsafeRunOnGraphicsCard $ OGLU.ortho2D (realToFrac a)
                                                         (realToFrac b)
                                                         (realToFrac c)
                                                         (realToFrac d)
{-# INLINE ortho2D #-}

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f $ g x y
{-# INLINE (.:) #-}

glDebugM :: String -> String -> GL ()
glDebugM = unsafeRunOnGraphicsCard .: debugM
{-# INLINE glDebugM #-}

glInfoM :: String -> String -> GL ()
glInfoM = unsafeRunOnGraphicsCard .: infoM
{-# INLINE glInfoM #-}

glNoticeM :: String -> String -> GL ()
glNoticeM = unsafeRunOnGraphicsCard .: noticeM
{-# INLINE glNoticeM #-}

glWarningM :: String -> String -> GL ()
glWarningM = unsafeRunOnGraphicsCard .: noticeM
{-# INLINE glWarningM #-}

glErrorM :: String -> String -> GL ()
glErrorM = unsafeRunOnGraphicsCard .: errorM
{-# INLINE glErrorM #-}

glCriticalM :: String -> String -> GL ()
glCriticalM = unsafeRunOnGraphicsCard .: criticalM
{-# INLINE glCriticalM #-}

glAlertM :: String -> String -> GL ()
glAlertM = unsafeRunOnGraphicsCard .: alertM
{-# INLINE glAlertM #-}

glEmergencyM :: String -> String -> GL ()
glEmergencyM = unsafeRunOnGraphicsCard .: emergencyM
{-# INLINE glEmergencyM #-}
