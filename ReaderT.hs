newtype ReaderT r m a = 
  ReaderT { runReaderT :: r -> m a }

instance Functor f => Functor (ReaderT r f) where
  fmap f (ReaderT rfa) = 
    ReaderT $ (fmap . fmap) f rfa

instance Applicative f => Applicative (ReaderT r f) where 
  pure = ReaderT . pure . pure
  (ReaderT rmab) <*> (ReaderT rma) = 
    ReaderT $ (<*>) <$> rmab <*> rma
  
instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> 
    rma r >>= \a -> 
      runReaderT (f a) r
  
