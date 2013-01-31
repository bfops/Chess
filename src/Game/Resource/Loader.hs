{-# LANGUAGE TupleSections #-}
-- | A generic resource loader, containing methods to load and unload resuorces
--   automatically given a list of resource that must be loaded "now", or
--   "eventually". See the documentation for 'ResourceLoader' for more
--   information.
module Game.Resource.Loader ( LoadableResource(..)
                            , ResourceRequest
                            , ResourceLoader
                            , emptyResourceLoader
                            , load
                            , preload
                            , chooseResources
                            , runDeferred
                            , getResource
                            ) where

import Prelewd

import Impure hiding (force)
import IO

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Data.IVar                    as IV
import           Data.Conduit                 as Cond
import qualified Data.Conduit.List            as C
import           Data.Hashable
import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as S
import           Data.HashString
import Data.Tuple
import           Game.Resource.Loadable
import           Graphics.Rendering.OpenGL.Monad
import Storage.List

-- | A load request for a hashed resource.
data ResourceRequest = ResourceRequest { mustLoad   :: Bool
                                       -- ^ If the Bool is True, load before the frame is rendered.
                                       , resourceId :: HashString
                                       -- ^ Hash of the resource to load.
                                       }
    deriving (Eq)

-- | The 'ResourceLoader' keeps track of all resources currently loaded in
--   memory and on the graphics card. It is specialized for every type of
--   loadable resource.
data ResourceLoader i r = ResourceLoader { loaded    :: M.HashMap HashString r
                                         , deferred  :: M.HashMap HashString i
                                         , preloaded :: M.HashMap HashString (PossiblyLoaded i r)
                                         }

data PossiblyLoaded i r = PartialLoad (IV.IVar (Maybe i))
                        | FullyLoaded r

instance NFData ResourceRequest where
    rnf = rnf . resourceId
    {-# INLINE rnf #-}

instance Hashable ResourceRequest where
    hashWithSalt = hashWithSalt <%> resourceId
    {-# INLINE hashWithSalt #-}

instance (NFData i, NFData r) => NFData (ResourceLoader i r) where
    rnf (ResourceLoader l d p) = rnf l `seq`
                                 rnf d `seq`
                                 rnf p `seq`
                                 ()

instance NFData r => NFData (PossiblyLoaded i r) where
    rnf (PartialLoad _) = ()
    rnf (FullyLoaded r) = rnf r
    {-# INLINE rnf #-}

instance NFData (IV.IVar a)

load :: HashString -> ResourceRequest
load = ResourceRequest True

preload :: HashString -> ResourceRequest
preload = ResourceRequest False

-- | Creates a new resource loader, with no resources loaded.
emptyResourceLoader :: LoadableResource i r => ResourceLoader i r
emptyResourceLoader = ResourceLoader M.empty M.empty M.empty

-- | Drops unrequested resources, and removes requests which have already been
--   fulfilled. Therefore, you will have successfully chosen all textures by
--   running a syncLoad on all 'Loaded' requests, and an asyncLoad on all
--   'Preload' textures.
--
--   Exit invariant: A given texture name exists in one, and only one of:
--
--                   the returned ResourceRequest list
--                   loaded
--                   deferred
--                   preloaded
--
--   Please see chooseTextureFlow.png for an explaination of the data flow
--   associated with this function. It's a tad bit complicated.
solveExistingResources :: (LoadableResource i r, NFData i, NFData r)
                       =>           ResourceLoader i r -> [ResourceRequest]
                       -> SystemIO (ResourceLoader i r  , [ResourceRequest])
solveExistingResources rl urs = do d' <- updateDeferred
                                   -- A texture is loaded if its a requested resource and in the existing
                                   -- loaded map, or exists in the preloaded map as a texture.
                                   --
                                   -- O(n) - 1 sync pass.
                                   let l' = M.fromList $ mapMaybe keepIfLoaded sync
                                   return $!! (ResourceLoader l' d' p', rs' l' d')
    where
        fromLoaded (FullyLoaded x) = Just x
        fromLoaded _ = Nothing

        fromPartial (PartialLoad x) = Just x
        fromPartial _ = Nothing

        keepIfLoaded req = (req,) <$> msum [ M.lookup req l, M.lookup req p >>= fromLoaded ]

        -- Returns false if the given request is a Preload request, and
        -- already exists as a load request in the given set.
        noDups :: S.HashSet ResourceRequest -> ResourceRequest -> Bool
        noDups s req = mustLoad req || not (S.member (load $ resourceId req) s)

        -- Resources, but with duplicate values filtered out.
        --
        -- If a resource existed as a 'Loaded' _and_ a 'Preload', the
        -- 'Loaded' clause is preferred.
        --
        -- O(n) - 2 passes.
        rsUnique :: S.HashSet ResourceRequest
        rsUnique = S.filter =<< noDups $ S.fromList urs

        -- Requests are only valid if they don't exist in one of the existing
        -- texture maps.
        --
        -- O(n) - 1 sync pass, 1 async pass.
        rs' :: LoadableResource i r
            => M.HashMap HashString r
            -> M.HashMap HashString i
            -> [ResourceRequest]
        rs' l' d' = let loadedTextures = S.fromList $ M.keys l' <> M.keys d' <> M.keys p'
                    in S.toList $ S.filter (not . flip S.member loadedTextures . resourceId) rsUnique

        -- A texture is deferred if its a requested resource and in the
        -- existing deferred map, or exists in the preload map as a promise.
        --
        -- O(n) - 2 sync passes, 1 async pass.
        updateDeferred = savePreloaded <$> mapM (IV.blocking . IV.read) alreadyPreloaded
            where
                savePreloaded preloadedVals = insertListM (S.foldr (keepDeferred . resourceId) M.empty rsUnique)
                                                          (zip preloadedKeys $ mapMaybe id preloadedVals)
                keepDeferred h m = maybe m (M.insert h `flip` m) $ M.lookup h d
                keepPartial req = (req,) <$> (M.lookup req p >>= fromPartial)

                -- only promote the synchronous requests from preload.
                (preloadedKeys, alreadyPreloaded) = unzip $ mapMaybe keepPartial sync

        -- A texture is preloaded if it has been requested as one, and is either
        -- already in 'loaded' or 'preloaded'.
        keepLoaded req = (req, ) <$> msum [ M.lookup req p, FullyLoaded <$> M.lookup req l ]

        p' = M.fromList $ mapMaybe keepLoaded async

        sync  = syncRequests  $ S.toList rsUnique
        async = asyncRequests $ S.toList rsUnique
        l = loaded    rl
        d = deferred  rl
        p = preloaded rl

-- | Ensures that the given list of resources is loaded. If a loaded resource
--   isn't in the list, it will be unloaded. The texture names are given as
--   'HashString' (not 'String'), so make sure to use the OverloadedStrings
--   extension.
--
--   This function will block until all 'Loaded' resources have finished
--   loading.
chooseResources :: (LoadableResource i r, NFData i, NFData r)
                => ResourceLoader i r
                -- ^ The loader we'll consult for already loaded textures so we
                --   can avoid double-allocations.
                -> [ResourceRequest]
                -- ^ A list of textures that will be required in the next
                --   render iteration.
                -> SystemIO (ResourceLoader i r)
                -- ^ A new resource loader, with all the required textures in
                --   place.
chooseResources = (loadNew =<<) <$$> solveExistingResources
    where loadNew (rl', reqs') = do s <- syncLoad (syncRequests reqs')
                                    a <- asyncLoad (asyncRequests reqs')
                                    return $ force (updateLists rl' s a)
          updateLists rl' = ResourceLoader (loaded rl') . insertListM (deferred rl') <%> updateAsync rl'
          updateAsync rl' = insertListM (preloaded rl') . map (map PartialLoad)

-- | Loads a list of textures by name, returning all successfully loaded
--   textures in a list, zipped with its name.
syncLoad :: (LoadableResource i r, NFData i)
         => [HashString]
         -> SystemIO [(HashString, i)]
syncLoad names = C.sourceList names
               $$ diskLoaderConduit
               =$ C.consume

-- | Returns the given textures as asynchronous promises, zipped with
--   their names.
asyncLoad :: LoadableResource i r
          => [HashString]
          -> SystemIO [(HashString, IV.IVar (Maybe i))]
asyncLoad = mapM $ \hs -> (hs,) <$> saveLoad hs
    where 
          saveLoad hs = IV.new >>= (<$) <*> void . forkIO . doLoad hs
          doLoad hs iv = IV.write iv =<< fromDisk (fromHashString hs)

cMapMaybe :: Monad m => (a -> Maybe b) -> Conduit a m b
cMapMaybe f = C.map f =$= C.concatMap (\m -> m <&> (:[]) <?> [])

-- | Converts resource names into loaded resources, zipped with their
--   original name.
diskLoaderConduit :: (LoadableResource i r, NFData i) => Conduit HashString SystemIO (HashString, i)
diskLoaderConduit = C.mapM doLoad =$= cMapMaybe keepSuccessful
    where doLoad name = (name,) <$> fromDisk (fromHashString name)
          keepSuccessful (name, t) = (name,) . force <$> t

-- | Uploads all deferred resources to graphics memory.
runDeferred :: LoadableResource i r
            => ResourceLoader i r
            -> GL (ResourceLoader i r)
runDeferred (ResourceLoader l d p) = uncurry (fmap . updateLoaded <%> toGraphics) . unzip $ M.toList d
    where
          updateLoaded x y = ResourceLoader (l `insertListM` zip x y) M.empty p

-- | Gets a resource from the loader.
getResource :: LoadableResource i r
            => ResourceLoader i r
            -> HashString
            -> Maybe r
            -- ^ Nothing if the resource hasn't been loaded (for any reason, such as file-not-found).
getResource l h = M.lookup h $ loaded l

-- | Returns only the synchronous requests' 'HashString's.
syncRequests :: [ResourceRequest] -> [HashString]
syncRequests = map resourceId . filter mustLoad
{-# INLINE syncRequests #-}

-- | Returns only the asynchronous requests' 'HashString's.
asyncRequests :: [ResourceRequest] -> [HashString]
asyncRequests = map resourceId . filter (not . mustLoad)
{-# INLINE asyncRequests #-}

infixl 3 `insertListM`

-- | Inserts a list of key-value pairs into a map.
insertListM :: (Hashable a, Ord a) => M.HashMap a b -> [(a, b)] -> M.HashMap a b
insertListM = foldl' (uncurry . \x y z -> M.insert y z x)
{-# INLINE insertListM #-}
{-# SPECIALIZE insertListM :: M.HashMap HashString b -> [(HashString, b)] -> M.HashMap HashString b #-}
