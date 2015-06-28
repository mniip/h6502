module H6502.Trans
    (
        H6502T(..)
    )
    where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Word

import H6502.Env
import H6502.State

-- | The 6502 monad transformer: encapsulates the state and the environment of a 6502 cpu, parametrised by the monad under which the memory operates, for example @'IO'@ or @'State' Memory@. Basically it is a @'ReaderT' 'H6502Environment' ('StateT' 'H6502State' m) a@, and the instances are identical.
newtype H6502T m a = H6502T { runH6502T :: H6502Environment m -> H6502State -> m (a, H6502State) }

instance Functor m => Functor (H6502T m) where
    fmap f (H6502T x) = H6502T $ (fmap (mapFst f) .) . x
        where mapFst f ~(a, s) = (f a, s)

instance (Functor m, Monad m) => Applicative (H6502T m) where
    pure x = H6502T $ \e s -> return (x, s)
    (H6502T f) <*> (H6502T x) = H6502T $ \e s -> do
        ~(f', s') <- f e s
        ~(x', s'') <- x e s'
        return (f' x', s'')

instance Monad m => Monad (H6502T m) where
    return x = H6502T $ \e s -> return (x, s)
    (H6502T k) >>= f = H6502T $ \e s -> do
        ~(k', s') <- k e s
        runH6502T (f k') e s'

instance MonadIO m => MonadIO (H6502T m) where
    liftIO m = H6502T $ \e s -> do
        m' <- liftIO m
        return (m', s)
