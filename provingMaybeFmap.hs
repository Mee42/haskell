

fmap (f . g) = fmap f . fmap g

instance Functor Maybe where
    fmap f (Just x) = Just $ f x
    fmap _ Nothing = Nothing


f :: a -> b
g :: b -> c

fmap (f . g) = fmap f . fmap g


input :: a

fmap (f . g) = fmap f . fmap g
-- haskell: applying the same input to the same function will result in the same return value
fmap (f . g) input = (fmap f . fmap g) $ input
fmap (f . g) input = fmap g (fmap f input)

-- now there is no partial application and this get's a bit easier


-- **case**
--   input = Just /**/

-- inline fmap call, using the func. that matches Just

Just $ (f . g) $ fromJust input = fmap g $ Just $ f $ fromJust input 
-- using fromJust call to get value of input out to pass to fmap?
--   it's expanded in the function arguments but it's not really expandable inline

--becuase the value on the right being passed to fmap is Just, 
--  we can use the same matched function again for the second fmop
Just $ (f . g) $ fromJust input = Just $ g $ fromJust $ Just $ f $ fromJust input

-- xxx $ fromJust $ Just $ yyy
-- is the same as
-- xxx $ yyy

Just $ (f . g) $ fromJust input = Just $ g $ f $ fromJust input

-- Just $ f $ fromJust a
-- is the same as
-- fmap f a
-- ohno we're back to fmap
-- okay maybe not that
-- hmm 
-- okay
-- Just is kinda like a function. 
-- the same rule applies, at least  
-- if you pass in the same value, you'll get the same value out
-- so if the two values being passed in are the same
-- the values after creating the Just will be the same
-- *there's a word for that or something. transitive property. I didn't listen during math class >.>*
-- therefore...
(f . g) $ fromJust input = g $ f $ fromJust input

-- now clearly `fromJust input` will always be the same thing
-- damn that's nice
-- haskell is so nice
-- so we can inline it, if we want

value = fromJust input
(f . g) value = g $ f value

-- and now we can move that stuff around...
f $ g value = g $ f value
-- what the fuck?
