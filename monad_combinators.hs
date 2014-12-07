-- sequencing, but ignore each result
-- sequence_' [] = return []
-- sequence_' (m:ms) = m >> \ _ -> sequence_' ms INVALID

sequence2_' [] = return ()
sequence2_' (m:ms) = (foldl (>>) m ms) >> return ()

sequence3_' ms = foldl (>>) (return ()) ms

sequence4_' [] = return ()
sequence4_' (m: ms) = m >> sequence4_' ms

sequence5_' [] = return ()
sequence5_' (m : ms) = m >>= \ _ -> sequence5_' ms

-- sequence6_' ms = foldr (>>=) (return ()) ms INVALID

sequence7_' ms = foldr (>>) (return ()) ms

sequence8_' ms = foldr (>>) (return []) ms


--  sequence, but collect all intermediate results
-- sequence' :: Monad m => [m a] -> m [a]

sequence1 [] = return []
sequence1 (m:ms) = m >>= \s ->
                    do ss <- sequence1 ms
                       return (s:ss)

sequence2  [] = return []
sequence2 ms = foldr func (return []) ms
      where
        func :: Monad m => m a -> m [a] -> m [a]
        func m acc
          = do a <- m
               as <- acc
               return (a:as)

sequence3 [] = return []
sequence3 (m:ms)
            = do x <- m
                 xs <- sequence3 ms
                 return (x:xs)

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
mapM1 f as = sequence (map f as)

mapM2 f [] = return []
mapM2 f (a:as) = f a >>= \b -> mapM f as >>= \bs -> return (b:bs)

mapM3 f [] = return []
mapM3 f (a:as) = do b <- f a
                    bs <- mapM3 f as
                    return (b:bs)

mapM4 f [] = return []
mapM4 f (a:as) = f a >>=
                  \b ->
                    do bs <- mapM4 f as
                       return (b:bs)

-- filterM :: Monad m => (a -> m Bool)  -> [a] -> m [a]
filterM _ [] = return []
filterM p (a:as) = do b <- p a
                      bs <- filterM p as
                      if b then return (a:bs) else return bs

-- liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m
  = do x <- m
       return (f x)

liftM2 f m = m >>= \a -> m >>= \b -> return (f a)
