module Data.Queue 


%default total

export
data Queue : Type -> Type where
    MkQueue : List a -> List a -> Queue a


export
empty : Queue a
empty = MkQueue [] []


export
isEmpty : Queue a -> Bool
isEmpty (MkQueue [] []) = True
isEmpty (MkQueue _ _ ) = False


export
singleton : a -> Queue a
singleton x = MkQueue [x] []

export
enqueue : a -> Queue a -> Queue a
enqueue x (MkQueue dequeueList enqueueList) =
    MkQueue dequeueList (x :: enqueueList)


export
dequeue : Queue a -> Maybe (a, Queue a)
dequeue (MkQueue [] []) = Nothing
dequeue (MkQueue (x::xs) enqueueList) = Just (x, MkQueue xs enqueueList)
dequeue (MkQueue [] enqueueList) =
    case reverse enqueueList of
        [] => Nothing
        (x :: xs) => Just (x, MkQueue xs [])


export
peek : Queue a -> Maybe a
peek (MkQueue [] []) = Nothing
peek (MkQueue (x::xs) _) = Just x
peek (MkQueue [] enqueueList) =
    case reverse enqueueList of
        [] => Nothing
        (x :: xs) => Just x
