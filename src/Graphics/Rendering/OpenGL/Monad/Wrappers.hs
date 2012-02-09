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
                                                , OGL.textureWrapMode
                                                , OGL.textureFilter
                                                , OGL.texture
                                                , OGL.textureFunction
                                                , OGL.textureBinding
                                                , OGL.matrixMode
                                                , vertex
                                                , texCoord
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
                                                ) where

import qualified Graphics.Rendering.OpenGL.GL as OGL
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

vertex :: OGL.Vertex a => a -> GL ()
vertex = unsafeRunOnGraphicsCard . OGL.vertex
{-# INLINE vertex #-}

texCoord :: OGL.TexCoord a => a -> GL ()
texCoord = unsafeRunOnGraphicsCard . OGL.texCoord
{-# INLINE texCoord #-}

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
loadIdentity = unsafeRunOnGraphicsCard $ OGL.loadIdentity
{-# INLINE loadIdentity #-}

swapBuffers :: GL ()
swapBuffers = unsafeRunOnGraphicsCard $ GLUT.swapBuffers
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
