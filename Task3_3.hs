module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

newtype PSet1 a = PSet1{ contains1 :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

--Monoid
-- Вариант 1: mappend реализуется как объединение множеств 

 

instance Semigroup (PSet1 a) where

   (<>) (PSet1 a) (PSet1 b) = PSet1 (\el -> (a el) || (b el))

instance Monoid (PSet1 a) where

  mempty = PSet1 (\el -> False)

-- Вариант 2: mappend реализуется как пересечение множеств

instance Semigroup (PSet2 a) where

   (<>) (PSet2 a) (PSet2 b) = PSet2 (\el -> (a el) && (b el))

instance Monoid (PSet2 a) where

      mempty = PSet2 (\el -> False)

-- Вариант 3: mappend реализуется как разность множеств (A\B)


instance Semigroup (PSet3 a) where

 
 (<>) (PSet3 a) (PSet3 b) = PSet3 (\el -> (a el) && (not $ b el))
-- Можно реализовать симметричную разность множеств как (A && not B) || (not A && B)

instance Monoid (PSet3 a) where

           mempty = PSet3 (\a -> False)

-- Functor
instance Functor PSet where
  fmap _ (PSet a) = PSet (\b -> False)
