module Utils where

maybeValue :: (Float -> Float -> Float) ->
              (Float -> Bool) ->
              Maybe Float ->
              Maybe Float ->
              Maybe Float
maybeValue op cond m1 m2 = case m1 of
                                Nothing -> Nothing
                                Just x ->
                                    case m2 of
                                        Nothing -> Nothing
                                        Just y -> if cond y then Just (op x y)
                                                    else Nothing

------------------------- List Aux -------------------------------
foldr1' :: (a -> b) -> (a -> b -> b) -> [a] -> b
foldr1' fbase _ [x] = fbase x
foldr1' fbase frec (x:xs) = frec x (foldr1' fbase frec xs)

thereIsEquivalent :: (a -> a -> Bool) -> a -> [a] -> Bool
thereIsEquivalent f e = foldr (\x r -> f e x || r) False

withoutRepeations :: (a -> [a] -> Bool) -> [a] -> [a]
withoutRepeations f = foldr (\x ys -> if f x ys then ys else x:ys) []

------------------------------------------------------------------
