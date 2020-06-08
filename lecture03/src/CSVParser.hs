module CSVParser where


-- Exercise: A CSV parser
-- A great example for the convenience and usefulness of monads is parser
-- combinators.  In this exercise, you will build a small parser combinator
-- library yourself.  In the end, the following code will, for example,
-- parse CSV files:

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

-- Try to understand the above code! The operator <* comes with the Applicative class.
-- Read the documentation to see what it does.

-- A value of type Parser a is a function that takes a string (the remaining input
-- at this point) and returns either Nothing, if parsing failed, or otherwise
-- returns a value of type a and the remaining input.
data Parser a = P (String -> Maybe (a, String))

-- to remove the P constructor. Use this rather than explicit pattern matches in the
-- functions below, as otherwise some recursive definitions will loop, because
-- pattern matching (and hence evaluation) happens too early.
runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p


extractElement :: Maybe (t, String) -> Maybe t
extractElement (Just (x, "")) = Just x
extractElement Nothing = Nothing
extractElement (Just (x, _)) = Nothing

extractRemainingString :: Maybe (t, String) -> String
extractRemainingString (Just (x, y)) = y
extractRemainingString Nothing = ""

-- `parse` is the main entry point to run a parser. It shall return successfully
-- only if the parser consumed the whole input, i.e. if the function inside
-- the Parser a returns a value of type a along with the empty string.
parse :: Parser a -> String -> Maybe a
parse parser input = extractElement $ (runParser parser input) 



-- Define a always failing Parser
-- parse noParser input == Nothing
noParser :: Parser a
noParser = P (\_ -> Nothing)



-- Define a parser that consumes no input and returns its argument.
-- You should have
-- parse (pureParser x) "" == Just x
-- xs ≠ "" ⇒ parse (pureParser x) xs == Nothing


pureParserFunction :: String -> Maybe (String, String)
pureParserFunction a = Just (a, a)


pureParser :: a -> Parser a
pureParser a = P (\x -> Just (a, x))



-- Declare Functor, Applicative and Monad for Parser
-- Remember all the laws for these?
-- All implementations should follow all the laws
instance Functor Parser where
    fmap f p = P p'
      where
        p' input = case runParser p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParser
    p1 <*> p2 = P $ \input -> do
        (f, rest1) <- runParser p1 input
        (x, rest2) <- runParser p2 rest1
        return (f x, rest2)

instance Monad Parser where
    return = pure
    p1 >>= k = P $ \input -> do
        (x, rest1) <- runParser p1 input
        runParser (k x) rest1


-- Parser that fails if the input is empty, and takes one character off the
-- input otherwise:
-- parse anyChar "" == Nothing
-- parse anyChar [c] == Just c
-- length xs > 1 ⇒ parse anyChar xs == Nothing


charParserFunction :: String -> Maybe (Char, String)
charParserFunction "" = Nothing
charParserFunction (x:xs) = Just (x, xs) 

anyChar :: Parser Char
anyChar = P (charParserFunction)


-- Parser which consumes a character matching
-- Note: without breaking the abstraction introduced by the Parser data type,
-- i.e. using only the combinators introduced above. You can use do-notation
-- if you want.
char :: Char -> Parser ()
char x = P $ \input -> do
    (element, rest) <- runParser anyChar input
    if element == x then return ((), rest) else return ((), input)


-- Parser which consumes any character not matching criteria
-- Note: without breaking the abstraction introduced by the Parser data type,
-- i.e. using only the combinators introduced above. You can use do-notation
-- if you want.


anyCharBut :: Char -> Parser Char
anyCharBut = undefined


-- Parser which tries the left parser. If it succeeds, it uses that, otherwise
-- it runs the second parser on its input. This implements backtracking in a
-- very naive way (so don’t expect this parser to have the best performance
-- characteristics – there are highly optimized parser libraries out there).
-- parse (noParser `orElse` p) input == parse p input
-- parse (pureParser x `orElse` p) input == parse (pureParser x) input
-- parse (anyChar `orElse` pureParser '☃') "" == Just '☃'
-- parse (anyChar `orElse` pureParser '☃') [c] == Just c
-- length xs > 1 ⇒ parse (anyChar `orElse` pureParser '☃') xs == Nothing
orElse :: Parser a -> Parser a -> Parser a
orElse = undefined




-- Parser combinator which applies the given parser as often as possible until
-- it fails, and then returns all results as a list.
-- Implement this again without breaking the abstraction, using the combinators
-- above.

many :: Parser a -> Parser [a]
many = undefined


-- Parser combinator so that p1 `sepBy` p2 applies the p1, then p2, then p1 and
-- so on. Succeeds if the very first invocation of p1 fails, returning the
-- empty string. Also succeeds if any invocation of p2 fails, returning the
-- results of all p1 invocations as a list.
-- Implement this again without breaking the abstraction, using the combinators
-- above.
sepBy :: Parser a -> Parser () -> Parser [a]
sepBy = undefined

