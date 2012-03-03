{-# LANGUAGE TupleSections #-}
module Game.Resource.Loader ( LoadableResource(..)
                            , ResourceLoader
                            , ResourceRequest(..)
                            , emptyResourceLoader
                            , chooseResources
                            , runDeferred
                            , getResource
                            ) where

import           Control.Applicative
import           Control.Arrow ( second )
import           Control.Concurrent
import           Control.DeepSeq
import           Data.IVar                    as IV
import           Data.Conduit                 as Cond
import qualified Data.Conduit.List            as C
import           Data.Hashable
import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as S
import           Data.HashString
import           Data.List                    as L
import           Data.Maybe
import           Game.Resource.Loadable
import           Graphics.Rendering.OpenGL.Monad

-- | A resource request is used to signify that the following resource is
--   required to render the current frame. A resource will begin loading when
--   it first appears as either a 'Preload' or 'Loaded', and the frame will
--   not be rendered until all 'Loaded' resources are fully resident and ready
--   to go.
data ResourceRequest = Preload HashString -- ^ Will begin loading, but there is
                                          --   no guarantee it will be done
                                          --   before the frame is rendered.
                     | Loaded  HashString -- ^ Guaranteed to be loaded before the
                                          --   frame is rendered.
    deriving (Eq, Ord)

-- | Pulls the 'HashString' out of a 'ResourceRequest'.
resourceId :: ResourceRequest -> HashString
resourceId (Preload h) = h
resourceId (Loaded  h) = h
{-# INLINE resourceId #-}

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
    rnf (Preload h) = rnf h
    rnf (Loaded  h) = rnf h
    {-# INLINE rnf #-}

instance Hashable ResourceRequest where
    hash = hash . resourceId
    {-# INLINE hash #-}

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
                       =>     [ResourceRequest] -> ResourceLoader i r
                       -> IO ([ResourceRequest],   ResourceLoader i r)
solveExistingResources urs rl = do d'' <- d'
                                   let rs'' = rs' l' d'' p'
                                    in return $!! (rs'', ResourceLoader l' d'' p')
    where
        -- Resources, but with duplicate values filtered out.
        --
        -- If a resource existed as a 'Loaded' _and_ a 'Preload', the
        -- 'Loaded' clause is preferred.
        --
        -- O(n) - 2 passes.
        rs :: S.HashSet ResourceRequest
        rs = let t = S.fromList urs
              in S.filter (noDups t) t
            where
                -- Returns false if the given request is a Preload request, and
                -- already exists as a load request in the given set.
                noDups :: S.HashSet ResourceRequest -> ResourceRequest -> Bool
                noDups _ (Loaded   _ ) = True
                noDups s (Preload rid) = not $ S.member (Loaded rid) s

        -- Requests are only valid if they don't exist in one of the existing
        -- texture maps.
        --
        -- O(n) - 1 sync pass, 1 async pass.
        rs' :: LoadableResource i r
            => M.HashMap HashString r
            -> M.HashMap HashString i
            -> M.HashMap HashString (PossiblyLoaded i r)
            -> [ResourceRequest]
        rs' l'' d'' p'' = S.toList $ S.filter (not . flip S.member loadedTextures . resourceId) rs
            where
                loadedTextures :: S.HashSet HashString
                loadedTextures = S.fromList (M.keys l'' ++ M.keys d'' ++ M.keys p'')

        -- A texture is loaded if its a requested resource and in the existing
        -- loaded map, or exists in the preloaded map as a texture.
        --
        -- O(n) - 1 sync pass.
        l' = uncurry (L.foldl' addIfPreloaded) $ L.foldl' addIfLoaded (M.empty, []) sync
            where
                addIfLoaded (m, xs) req = case M.lookup req l of
                                            Just tex -> (M.insert req tex m, xs)
                                            Nothing  -> (m, req:xs)
                addIfPreloaded m req = case M.lookup req p of
                                          Just (FullyLoaded tex) -> M.insert req tex m
                                          _                      -> m

        -- A texture is deferred if its a requested resource and in the
        -- existing deferred map, or exists in the preload map as a promise.
        --
        -- O(n) - 2 sync passes, 1 async pass.
        d' = do preloadedVals <- mapM (IV.blocking . IV.read) alreadyPreloaded
                let zipped = zip preloadedKeys $ catMaybes preloadedVals
                 in return $ S.foldl' (\m r -> addIfDeferred (resourceId r) m) M.empty rs -- not just sync. See the diagram.
                           `insertListM` zipped
            where
                addIfDeferred req m = case M.lookup req d of
                                         Just img -> M.insert req img m
                                         Nothing  -> m
                -- only promote the synchronous requests from preload.
                (preloadedKeys, alreadyPreloaded) = unzip $ L.foldl' addIfPartial [] sync
                    where
                        addIfPartial xs req = case M.lookup req p of
                                                 Just (PartialLoad iv) -> (req, iv):xs
                                                 _                     -> xs

        -- A texture is preloaded if it has been requested as one, and is either
        -- already in 'loaded' or 'preloaded'.
        p' = uncurry (L.foldl' fromLoaded) $ L.foldl' fromPreloaded (M.empty, []) async
            where
                fromLoaded m req = case M.lookup req l of
                                      Just tex -> M.insert req (FullyLoaded tex) m
                                      Nothing  -> m
                fromPreloaded (m, xs) req = case M.lookup req p of
                                               Just x  -> (M.insert req x m, xs)
                                               Nothing -> (m, req:xs)

        sync  = syncRequests  $ S.toList rs
        async = asyncRequests $ S.toList rs
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
                -> IO (ResourceLoader i r)
                -- ^ A new resource loader, with all the required textures in
                --   place.
chooseResources rl reqs = do (reqs', rl') <- solveExistingResources reqs rl
                             newlyLoaded  <- syncLoad  $ syncRequests reqs'
                             asyncLoaded  <- asyncLoad $ asyncRequests reqs'
                             return $!! ResourceLoader (loaded rl')
                                                       (deferred  rl' `insertListM` newlyLoaded)
                                                       (preloaded rl' `insertListM` map (second PartialLoad) asyncLoaded)

-- | Loads a list of textures by name, returning all successfully loaded
--   textures in a list, zipped with its name.
syncLoad :: (LoadableResource i r, NFData i)
         => [HashString]
         -> IO [(HashString, i)]
syncLoad names = runResourceT $ C.sourceList names
                             $$ diskLoaderConduit
                             =$ C.consume

-- | Returns the given textures as asynchronous promises, zipped with
--   their names.
asyncLoad :: LoadableResource i r
          => [HashString]
          -> IO [(HashString, IV.IVar (Maybe i))]
asyncLoad   []      = return []
asyncLoad (name:xs) = do iv <- IV.new
                         _  <- forkIO $ IV.write iv =<< fromDisk (fromHashString name)
                         ((name, iv):) <$> asyncLoad xs

-- | Converts resource names into loaded resources, zipped with their
--   original name.
diskLoaderConduit :: (LoadableResource i r, NFData i) => Conduit HashString IO (HashString, i)
diskLoaderConduit = C.mapM (\name -> (name,) <$> fromDisk (fromHashString name)) -- Load the resources, keep the name.
                  -- Replace the (Text, Maybe ..) with Maybe (Text, ...)
                 =$= C.map (\(name, tex) -> (name,) <$> tex)
                  -- Only keep successfully loaded textures.
                 =$= C.filter isJust =$= C.map (force . fromJust)

-- | Uploads all deferred resources to graphics memory.
runDeferred :: LoadableResource i r
            => ResourceLoader i r
            -> GL (ResourceLoader i r)
runDeferred (ResourceLoader l d p) = let (ns, is) = unzip $ M.toList d
                                      in do rs <- toGraphics is
                                            return $ ResourceLoader
                                              (l `insertListM` zip ns rs)
                                              M.empty
                                              p

-- | Gets a resource from the loader. If Nothing is returned, then either the
--   resource does not exist, or it has not been loaded yet.
getResource :: LoadableResource i r
            => ResourceLoader i r
            -> HashString
            -> Maybe r
getResource rl name = M.lookup name $ loaded rl

-- | Returns only the synchronous requests' 'HashString's.
syncRequests :: [ResourceRequest] -> [HashString]
syncRequests = foldr filt []
    where
        filt rr soFar = case rr of
                          Loaded hs -> hs:soFar
                          Preload _ -> soFar
{-# INLINE syncRequests #-}

-- | Returns only the asynchronous requests' 'HashString's.
asyncRequests :: [ResourceRequest] -> [HashString]
asyncRequests = foldr filt []
    where
        filt rr soFar = case rr of
                            Preload hs -> hs:soFar
                            Loaded  _  -> soFar
{-# INLINE asyncRequests #-}

infixl 3 `insertListM`

-- | Inserts a list of key-value pairs into a map.
insertListM :: (Hashable a, Ord a) => M.HashMap a b -> [(a, b)] -> M.HashMap a b
insertListM = L.foldl' ins
    where
        ins t (k, x) = M.insert k x t
{-# INLINE insertListM #-}
{-# SPECIALIZE insertListM :: M.HashMap HashString b -> [(HashString, b)] -> M.HashMap HashString b #-}
