module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`
instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad (f . a)

instance Applicative FunMonad where
    pure a = FunMonad (\str -> a)
    (<*>) (FunMonad f) (FunMonad a) = FunMonad (\str -> (f str) (a str))

instance Monad FunMonad where
    return a = FunMonad(\str -> a)
    (>>=) (FunMonad a) f = FunMonad (\str -> fun (f (a str)) str)
