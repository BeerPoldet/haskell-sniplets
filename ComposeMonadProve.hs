data Compose f g a = Compose {
  getCompose :: f (g a)
}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose fgab) <*> (Compose fga) = Compose $ (<*>) <$> fgab <*> fga

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure

  (Compose fga) >>= f = 
    Compose $ fga >>= \ga -> ga >>= \a -> getCompose $ f a
  -- This is impossible to implement,     ^^^^^^^^^^^^^^^^
  -- I cannot unwrap f (g a)              here to get just g a.
  -- Monad alone is not enough to puzzle this.
  -- To solve is to actually know the concret type of g
  -- This is the beginning of Monad Transformer

