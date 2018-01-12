not' :: Bool -> Bool
not' a = case a of
    True -> False
    False -> True 

absInt n =
    case (n >= 0) of
        True -> n
        _    -> -n
