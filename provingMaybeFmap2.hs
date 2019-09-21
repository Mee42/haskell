
input :: Maybe a
f :: a -> b
g :: b -> c

fmap (f . g) = fmap f . fmap g

-- add in a static value - input

fmap (f . g) input = (fmap f . fmap g) input

-- move stuff around

fmap (f . g) input = fmap f (fmap g input)

-- okay.. now?
-- hmm
-- how do we get rid of fmap

-- CASE:
x :: a
input = Just a


Just $ (f . g) x = Just $ f $ fromMaybe (Just (g x))

-- using fromMaybe and Just temporarilly so it makes a bit more sense?
-- fromJust input = x, so we don't bother there.

Just $ (f . g) x = Just $ f (g x)

-- now we extrapolate the point-free notation so I know what's actually going on

Just $ f (g x) = Just f (g x)

-- oh my god! it's right!
-- :party:


-- oh and just because we need to do this.
-- CASE:
input = Nothing a

fmap (f . g) input = fmap f (fmap g input)
-- fmap f Nothing = Nothing (look at the def.)\
Nothing = fmap f Nothing
-- and again...
Nohting = Nothing
-- :DD

