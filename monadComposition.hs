-- After some experimentation it looks to me like the only thing I need to compose two arbitrary monads is a natural transformation between them
-- Let me try some things out
join n = n >>= id
-- bindFromJoin :: (M (M a) -> M a) -> M b -> (b -> M c) -> M c
bindFromJoin join m f = join $ fmap f m

-- Our natural transformation from list to maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x
alpha = safeHead

instance Functor MaybeList where
  fmap f x = MaybeList (fmap (fmap f) $ getMaybeList x)
 
instance Applicative MaybeList where
   pure = MaybeList . pure . pure 
   f <*> v = do
      f' <- f
      v' <- v
      return $ f' v'

joinForMaybeList :: MaybeList (MaybeList x) -> MaybeList x
joinForMaybeList
  =  MaybeList . join . (fmap getMaybeList) . join . fmap alpha . getMaybeList

newtype MaybeList a = MaybeList {getMaybeList :: Maybe [a]} 
                      deriving (Show)

instance Monad MaybeList where
  return = MaybeList . return . return
  (>>=) = bindFromJoin joinForMaybeList

-- Let's setup a scenario with functions returning MaybrList
getStudentsFromClass :: Int -> MaybeList String
getStudentsFromClass 200 = MaybeList . Just $ ["Mary", "Jared"]
getStudentsFromClass 100 = MaybeList . Just $ ["John", "Bill"]
-- getStudentsFromClass 400 = MaybeList . Just $ []
getStudentsFromClass _ = MaybeList Nothing

getIt :: MaybeList String
getIt = do
  representative100 <- getStudentsFromClass 100
  representative200 <- getStudentsFromClass 200
  MaybeList $ Just [representative100, representative200]

main = putStrLn (show getIt)

