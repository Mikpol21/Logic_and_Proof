{-# LANGUAGE LambdaCase #-}

module Logic_Parser where

import Data.Char(isDigit,isSpace,ord,chr)
import Control.Applicative ( Alternative(..) )
import Control.Monad ( MonadPlus(..) )
import Propositions ( Conjecture(Proves), Prop(..) )


--------  Parser instances  --------

newtype Parser a = Parser { parse :: String -> [(a, String)] }
runParser :: Parser a -> String -> a
runParser m s = case complete of
    x:xs -> fst x
    []   -> if not . null $ res 
            then error "Parser did not consume entire stream."
            else error "Parser error."
    where   res = parse (spaces >> m) s
            complete = filter (null . snd) res

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

(<?>) :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
c <?> p = do
        x <- p
        if c x then return x else empty

chainl, chainr :: (Monad p, Alternative p) => p (a -> a -> a) -> p a -> p a
chainl op p = foldl (flip ($)) <$> p <*> many (flip <$> op <*> p)
chainr op p = do
    xs <- many $ flip ($) <$> p <*> op
    e <- p
    return $ foldr ($) e xs

--------  Parser combinators  --------

char :: Parser Char
char = Parser $ \case
  "" -> []
  c : cs -> [(c, cs)]

oneOf :: String -> Parser Char
oneOf xs = flip elem xs <?> char

oneOfs :: [String] -> Parser String
oneOfs = foldl (<|>) empty . map symbol

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

word :: Parser String
word = token $ some . oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789"

--------  Parsing Propositions  --------

eqvSymbols, impSymbols, orSymbols, andSymbols, negationSymbols, contradictionSymbols :: [String]
eqvSymbols = ["<=>", "eqv", "<->", "???"]
impSymbols = ["->", "=>", "implies", "imp", "???"]
orSymbols = ["||", "or", "v", "+", "???"]
andSymbols = ["&", "&&", "and", "*", ".", "^", "???"]
negationSymbols = ["not", "~", "!", "??"]
contradictionSymbols = ["F", "contradiction", "bottom", "0", "???"]

impOp, orOp, andOp, eqvOp :: Parser (Prop -> Prop -> Prop)
eqvOp = oneOfs eqvSymbols >> return Eqv
impOp = oneOfs impSymbols >> return Imp
orOp = oneOfs orSymbols >> return Or
andOp = oneOfs andSymbols >> return And


equivalenceParser, implicationParser, disjunctionParser, conjunctionParser :: Parser Prop
equivalenceParser = eqvOp `chainr` implicationParser
implicationParser = impOp `chainr` disjunctionParser
disjunctionParser = orOp `chainl` conjunctionParser
conjunctionParser = andOp `chainl` termsParser

atomicsParser, termsParser, negationParser, contradictionParser, propParser :: Parser Prop
atomicsParser = Atomic <$> word
contradictionParser = oneOfs contradictionSymbols >> return Contradiction
negationParser =  oneOfs negationSymbols >> Not <$> termsParser
termsParser = contradictionParser <|> negationParser <|> atomicsParser <|> parens equivalenceParser
propParser = equivalenceParser

--------  Conjectures  --------

teeSymbols :: [String]
teeSymbols =  ["yields", "proves", "prove", "satisfies", "entails", "???"]

teeOp :: Parser ([Prop] -> [Prop] -> Conjecture)
teeOp = oneOfs teeSymbols >> return Proves

conjectureParser :: Parser Conjecture
conjectureParser = flip ($) <$> propsParser <*> teeOp <*> propsParser 
                  --  <|> flip ($) [] <$> teeOp <*> propsParser

propsParser :: Parser [Prop]
propsParser = ((:) <$> propParser <*> many (symbol "," >> propParser)) <++> (spaces >> return [])