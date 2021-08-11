>{-# LANGUAGE LambdaCase #-}
> module Lecture16 where

> import Data.Char(isDigit,isSpace,ord,chr)

Functors
~~~~~~~~

Recall that a functor is a type contructor with a map function

  class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    (<$) = fmap . const

  (<$>) = fmap

that respects composition

  (id<$>)    = id
  ((f.g)<$>) = (f<$>).(g<$>)

For example

  instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

  instance Functor [] where
    fmap f    []  = []
    fmap f (x:xs) = f x : fmap f xs


Applicatve Functors
~~~~~~~~~~~~~~~~~~~

An applicative functor is one with an application-like operation

  class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

that should obey these laws

  pure id <*> x = x
  pure (.) <*> f <*> g <*> x = f <*> (g <*> x)
  pure f <*> pure x = pure (f x)
  f <*> pure x = pure ($ x) <*> f

where ($ x) f = (f $ x) = f x

The <$> on the underlying functor should be

  f <$> x = pure f <*> x

For example

  instance Applicative Maybe where
    pure = Just
    Just f <*> Just x = Just (f x)
    _      <*> _      = Nothing

  intance Applicative [] where
    pure x    = [ x ]
    fs <*> xs = [ f x | f <- fs, x <- xs ]

Monads
~~~~~~

A Monad is an applicative functor with a bind operation (>>=)

  class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    m >> k = m >>= const k

where in the underlying applicative functor

  pure = return
  fs <*> xs = fs >>= (\f -> xs >>= (\x -> return (f x)))

or, in do-notation
  
  fs <*> xs = do { f <- fs; x <- xs; return (f x) }

and bind should obey these laws

  return x >>= k  =  k x
  j >>= return  =  j
  j >>= (\x -> k x >>= h)  =  (j >>= k) >>= h

or, in do-notation

  do { y <- return x; k y } = k x
  do { x <- j; return x } = j
  do { x <- j; do { y <- k x; h y }} = do { y <- do { x <- j; k x }; h y }

For example

  instance Monad Maybe where
    return = pure
    Nothing >>= _ = Nothing
    Just x  >>= k = k x

  instance Monad [] where
    return = pure
    xs >>= f = [ y | x <- xs, y <- f x ]



Monadic join (equivalent of concat)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Monads may also be specified by defining a join operation (and return)

> join :: Monad m => m (m a) -> m a
> join x = x >>= id

from which bind can be derived by

  m >>= f = join (f <$> m)



Kleisli composition (from Control.Monad)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An alternative formulation of monads uses the composition operator

> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
> (f >=> g) x = f x >>= g

which is so defined that

  m >>= k = (id >=> k) m

The Monad laws written in terms of Kleisli composition

  return >=> g  =  g
  f >=> return  =  f
  f >=> (g >=> h) = (f >=> g) >=> h

make it clear that return is the unit of composition, and that
composition is associative


Monads with failure
~~~~~~~~~~~~~~~~~~~

If the Monad has a "zero" operation which represents failure,
and which is a left-zero of bind.

The real Control.Monad.mzero is in Control.Monad.MonadPlus

> class Monad m => MonadZero m where
>   mzero :: m a

you can define an equivalent of filter (standardly, called mfilter)

> (<?>) :: MonadZero m => (a -> Bool) -> m a -> m a
> c <?> p = do { x <- p; if c x then return x else mzero }

> instance MonadZero Maybe where
>   mzero = Nothing

> instance MonadZero [] where
>   mzero = []
 
 
Monads with composition
~~~~~~~~~~~~~~~~~~~~~~~

mplus should be associative, and mzero should be its unit

> class MonadZero m => MonadPlus m where
>   mplus :: m a -> m a -> m a


> instance MonadPlus [] where
>   mplus = (++)

> instance MonadPlus Maybe where
>   Nothing `mplus` r = r
>   l       `mplus` _ = l


Alternative 
~~~~~~~~~~~

(<|>) should be associative, and empty should be its unit 

> class Applicative f => Alternative f where
>   empty :: f a
>   (<|>) :: f a -> f a -> f a

>   some :: f a -> f [a]
>   some v = (:) <$> v <*> many v

>   many :: f a -> f [a]
>   many v = some v <|> pure []


> chain :: Alternative p => p (a->a->a) -> p a -> p a
> chain op p = accumulate <*> p <*> many (flip <$> op <*> p)
>              where accumulate = pure (foldl (flip ($))) 


> instance Alternative Maybe where
>   empty = mzero
>   (<|>) = mplus

> instance Alternative [] where
>   empty = mzero
>   xs <|> ys = take 1 (xs `mplus` ys)




An example of a complete suite of definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data Mebe a = Nowt | Juss a

> instance Functor Mebe where
>   fmap f  Nowt    = Nowt
>   fmap f (Juss x) = Juss (f x)

> instance Applicative Mebe where
>   pure = Juss
>   Juss f <*> Juss x = Juss (f x)
>   _      <*> _      = Nowt

> instance Monad Mebe where
>   return = Juss
>   Nowt   >>= f = Nowt
>   Juss x >>= f = f x

> instance MonadZero Mebe where
>   mzero = Nowt

> instance MonadPlus Mebe where
>   Nowt `mplus` r = r
>   l    `mplus` _ = l





Parser as a Monad
~~~~~~~~~~~~~~~~~

> newtype Parser a = Parser { parse :: String -> [(a, String)] }

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\xs -> [ (f x, ys) | (x,ys) <- p xs ])

> instance Applicative Parser where
>   pure a = Parser (\xs -> [(a,xs)])
>   Parser p <*> Parser q = 
>       Parser (\xs -> [ (f x, zs) | (f,ys) <- p xs, (x,zs) <- q ys ] )

> instance Alternative Parser where
>   empty = Parser (const [])
>   Parser p <|> Parser q = Parser (\cs -> p cs <|> q cs)

> instance Monad Parser where
>   return = pure
>   Parser p >>= f  = Parser (\xs ->
>          [ (v,zs) | (a,ys) <- p xs, (v,zs) <- f a `parse` ys ])

> instance MonadZero Parser where
>   mzero = Parser (const [])

> instance MonadPlus Parser where
>   Parser p `mplus` Parser q = Parser (\cs -> p cs ++ q cs)




The parser example from Lecture 15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> char :: Parser Char
> char = Parser $ \ case 
>     "" -> []
>     (c:cs) -> [(c,cs)]

> character :: Char -> Parser Char
> character c = (c==) <?> char

> string :: String -> Parser String
> string    ""  = return ""
> string (c:cs) = do { x <- character c; xs <- string cs; return (x:xs) }

> space :: Parser String
> space = many (isSpace <?> char)

> token :: Parser a -> Parser a
> token p = do { x <- p; space; return x }

> symbol :: String -> Parser String
> symbol xs = token (string xs)

> digits :: Parser Int
> digits = foldl shift 0 <$> token (some digit)
>          where shift n d = 10*n+d

> digit :: Parser Int
> digit = value <$> (isDigit <?> char)
>         where value x = ord x - ord '0'

> expr, term, factor :: Parser Expr
> expr   = addop `chain` term
> term   = mulop `chain` factor
> factor = number <|> parens "(" expr ")"

> addop, mulop :: Parser (Expr -> Expr -> Expr)
> addop = (App Add <$ symbol "+") <|> (App Sub <$ symbol "-")
> mulop = (App Mul <$ symbol "*") <|> (App Div <$ symbol "/")

> number :: Parser Expr
> number = Val <$> digits

> parens :: String -> Parser a -> String -> Parser a
> parens open p close = do { symbol open; e <- p; symbol close; return e }




> from :: Parser a -> String -> Maybe a
> from p xs = unique (complete (parse (space >> p) xs))
>              where unique [v] = Just v
>                    unique  _  = Nothing
>                    complete ps = [ v | (v, xs) <- ps, null xs ]


> data Expr = Val Int | App Op Expr Expr
> data Op = Add | Sub | Mul | Div

> instance Show Expr where
>   show = exp
>          where exp (Val n)     = show n
>                exp (App o l r) = par l ++ show o ++ par r
>                par (Val n)     = show n
>                par e           = "(" ++ exp e ++ ")"

> instance Show Op where
>   show Add = "+"
>   show Sub = "-"
>   show Mul = "*"
>   show Div = "/"

> eval :: Expr -> Int
> eval (Val n)     = n
> eval (App o l r) = eval l `op` eval r
>      where op = case o of
>                   Add -> (+)
>                   Sub -> (-)
>                   Mul -> (*)
>                   Div -> div