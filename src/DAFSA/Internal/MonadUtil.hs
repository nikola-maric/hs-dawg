module DAFSA.Internal.MonadUtil where

import Data.Bool (bool)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = ifM c t (pure ())
{-# INLINE whenM #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t f = c >>= bool f t
{-# INLINE ifM #-}