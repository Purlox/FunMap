module FunMap (
    FunMap(),
    
    empty,
    singleton,
    
    (!),
    at,
    
    mapKeys,
    mapKeys',
    mapKeysR,
    mapKeysR',
    
    insert,
    delete,
    
    toList
) where

import Control.Monad ((>=>))

-- | A simple map between key and values.
--
-- It's implemented as a function.
data FunMap k v = FunMap (k -> Maybe v)

instance (Show k, Show v, Enum k) => Show (FunMap k v) where
    show = ("fromList " ++) . show . toList

instance Functor (FunMap k) where
    fmap g (FunMap f) = FunMap (fmap g . f)


-- | Creates an empty FunMap
empty :: FunMap k v
empty = FunMap f
    where f _ = Nothing


-- | Creates a FunMap with a single element inside it.
singleton :: (Eq k) => k -> v -> FunMap k v
singleton key value = FunMap f
    where f x | x == key  = Just value
              | otherwise = Nothing


-- | Inserts an element into the FunMap.
--
-- Overwrites earlier elements if they share the same keys.
insert :: (Eq k) => k -> v -> FunMap k v -> FunMap k v
insert key value (FunMap f) = FunMap g
    where g x | x == key  = Just value
              | otherwise = f x


-- | Deletes an element out of the FunMap.
delete :: (Eq k) => k -> FunMap k v -> FunMap k v
delete key (FunMap f) = FunMap g
    where g x | x == key  = Nothing
              | otherwise = f x


-- | Returns an element from the FunMap if it's in there.
--
-- Throws an error if it isn't.
at :: FunMap k v -> k -> v
FunMap f `at` x = maybe err id $ f x
    where err = error "Error: element with key" ++ x ++ "not found in the FunMap."


-- | Safe way to return an element from the FunMap.
(!) :: FunMap k v -> k -> Maybe v
FunMap f ! x = f x


-- | Maps a function over the keys.
-- 
-- The supplied function must be bijective.
mapKeys :: (k2 -> k1) -> FunMap k1 v -> FunMap k2 v
mapKeys g (FunMap f) = FunMap (f . g)


-- | Maps a function over the keys.
-- 
-- The supplied function must be bijective with regards to k1 and k2, so it doesn't matter if a lot of values get mapped to Nothing. Also the domain of k1 should be as small as possible.
mapKeys' :: (k2 -> Maybe k1) -> FunMap k1 v -> FunMap k2 v
mapKeys' g (FunMap f) = FunMap (g >=> f)


-- | Maps a function over the keys
-- 
-- The supplied function must be bijective and also the domain of k1 should be as small as possible.
mapKeysR :: (Enum k1, Eq k2) => (k1 -> k2) -> FunMap k1 v -> FunMap k2 v
mapKeysR g (FunMap f) = FunMap (f . invertFunction g)


-- | Maps a function over the keys
-- 
-- The supplied function must be bijective with regards to k1 and k2, so it doesn't matter if a lot of values get mapped to Nothing. Also the domain of k1 should be as small as possible.
mapKeysR' :: (Enum k1, Eq k2) => (k1 -> Maybe k2) -> FunMap k1 v -> FunMap k2 v
mapKeysR' g (FunMap f) = FunMap (invertFunction' g >=> f)


toList :: (Enum k) => FunMap k v -> [(k, v)]
toList (FunMap f) = [ (x, value) | x <- enumAll, Just value <- [f x] ]


-- ##########################
-- ####  Help functions  ####
-- ##########################

-- | Inverts a bijective function in the mathematical sense.
invertFunction :: (Enum a, Eq b) => (a -> b) -> (b -> a)
invertFunction f x = case lookup x $ fmap (\y -> (f y, y)) enumAll of
                         Just result -> result
                         Nothing     -> error "Error: Unable to invert function"


-- | Inverts a bijective function in the mathetamical sense.
invertFunction' :: (Enum a, Eq b) => (a -> Maybe b) -> (b -> Maybe a)
invertFunction' f x = lookup x [ (r, e) | e <- enumAll, Just r <- [f e] ]


enumAll :: (Enum e) => [e]
enumAll = enumFrom (toEnum 0)

