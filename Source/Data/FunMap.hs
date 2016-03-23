module FunMap (
    empty,
    singleton,
    
    insert,
    delete,
    
    toList
) where

data FunMap k v = FunMap (k -> Maybe v)


empty :: FunMap k v
empty = FunMap f
    where f _ = Nothing


singleton :: (Eq k) => k -> v -> FunMap k v
singleton key value = FunMap f
    where f x | x == key  = Just value
              | otherwise = Nothing


insert :: (Eq k) => k -> v -> FunMap k v -> FunMap k v
insert key value (FunMap f) = FunMap g
    where g x | x == key  = Just value
              | otherwise = f x


delete :: (Eq k) => k -> FunMap k v -> FunMap k v
delete key (FunMap f) = FunMap g
    where g x | x == key  = Nothing
              | otherwise = f x


toList :: (Enum k) => FunMap k v -> [(k, v)]
toList (FunMap f) = [ (x, value) | x <- enumAll, Just value <- [f x] ]
    where enumAll = enumFrom (toEnum 0)

