{-# LANGUAGE LambdaCase #-}
import Data.Char(isDigit,isSpace,ord,chr)
import Control.Applicative
import Control.Monad
import Propositions

-------- Parser type --------

newtype Parser a = Parser { parse :: String -> [(a, String)] }
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Monad Parser where
    return = pure
    -- p :: String -> [(a, String)]
    -- f :: a -> Parser a (String -> [(a, String)])
    Parser p >>= f = Parser $ \xs -> do
        (a, ys) <- p xs
        f a `parse` ys

instance Functor Parser where
    fmap f (Parser p) = Parser (\xs -> [(f x, xs) | (x, xs) <- p xs])

instance Applicative Parser where
    pure a = Parser $ \xs -> [(a,xs)]
    -- (<*>) :: f (a -> b) -> f a -> f b
    Parser p <*> Parser q = Parser $ \xs -> do
        (f, ys) <- p xs
        (x, zs) <- q ys
        return (f x, zs)

instance Alternative Parser where
    empty = Parser $ const []
    --Parser p <|> Parser q = Parser $ \cs -> p cs <|> q cs
    Parser p <|> Parser q = Parser $ \cs -> case p cs of [] -> q cs ; x -> x

instance MonadPlus Parser where
    mzero = empty
    Parser p `mplus` Parser q = Parser $ \cs -> p cs ++ q cs
(<++>) :: MonadPlus m => m a -> m a -> m a
(<++>) = mplus
---------------------------

(<?>) :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
c <?> p = do
        x <- p
        if c x then return x else empty
{-
some :: (Alternative f, Applicative f) => f a -> f [a]
some v = pure (:) <*> v <*> many v

many :: (Alternative f, Applicative f) => f a -> f [a]
many v = some v <|> pure []
-}

chain, chainr :: Alternative p => p (a -> a -> a) -> p a -> p a
chain op p = foldl (flip ($)) <$> p <*> many (flip <$> op <*> p)
chainr op p = foldr ($) <$> p <*> many (flip <$> op <*> p)
-------- Lets go --------

char :: Parser Char
char = Parser $ \case
  "" -> []
  c : cs -> [(c, cs)]

oneOf :: [Char] -> Parser Char
oneOf xs = flip elem xs <?> char

character :: Char -> Parser Char
character c = (c == ) <?> char

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
    x <- character c
    xs <- string cs
    return (x:xs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { x <- p; spaces; return x }

parens :: Parser a -> Parser a
parens m = do
  symbol "("
  n <- m
  symbol ")"
  return n

symbol :: String -> Parser String
symbol xs = token (string xs)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = symbol x >> return f

unaryOp :: String -> (a -> a) -> Parser (a -> a)
unaryOp x f = symbol x >> return f

word :: Parser String
word = token $ some . oneOf $ ['a'..'z'] ++ "0123456789"

----- Precedence
--  1. Parethesis
--  2. Negation
--  3. Conjuction
--  4. Disjunction
--  5. Implication
--  6. Equivalence

eqvSymbols, impSymbols, orSymbols, andSymbols, negationSymbols, contradictionSymbols :: [String] 
eqvSymbols = ["<=>", "eqv", "<->"]
impSymbols = ["->", "=>", "imp", "implies"]
orSymbols = ["||", "or", "v", "+"]
andSymbols = ["&", "&&", "and", "*", ".", "^"]
negationSymbols = ["not", "~", "!"]
contradictionSymbols = ["F", "contradiction", "bottom", "0"]

eqvOp, impOp, orOp, andOp :: Parser (Prop -> Prop -> Prop)
eqvOp = infixOp "<->" Eqv
impOp = infixOp "->" Imp
orOp = infixOp "||" Or
andOp = infixOp "&" And


equivalences, implications, disjunctions, conjunctions :: Parser Prop
equivalences = eqvOp `chainr` implications
implications = impOp `chainr` disjunctions
disjunctions = orOp `chain` conjunctions
conjunctions = andOp `chain` terms

atomics, terms, negation :: Parser Prop
atomics = Atomic <$> word
negation = unaryOp "~" Not <*> terms
terms = atomics <|> negation <|> parens equivalences




run :: String -> Prop
run = runParser equivalences
testing :: String
testing = "~p & q & r -> r  ->     ~(a -> b)<-> ~p || q"