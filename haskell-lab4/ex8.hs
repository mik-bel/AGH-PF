
module Queue
( Queue
, emptyQ   -- :: Queue a
, isEmptyQ -- :: Queue a -> Bool
, addQ     -- :: a -> Queue a -> Queue a
, remQ     -- :: Queue a -> (a, Queue a)
) where

emptyQ   :: Queue a
isEmptyQ :: Queue a -> Bool
addQ     :: a -> Queue a -> Queue a
remQ     :: Queue a -> (a, Queue a)

newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue s) = null s
addQ a (MkQueue s) = MkQueue (s ++ [a])
remQ (MkQueue (s:ss)) = (s, MkQueue ss) 
    