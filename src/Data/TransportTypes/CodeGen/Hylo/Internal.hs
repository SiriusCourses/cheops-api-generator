module Data.TransportTypes.CodeGen.Hylo.Internal where
import Control.Arrow ((>>>))

newtype Term f =
    In
        { out :: f (Term f)
        }

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = f >>> fmap (ana f) >>> In

hylo :: (Functor f) => Coalgebra f a -> Algebra f b -> a -> b
hylo f g = ana f >>> cata g