module NameConf where

import           Control.Applicative            ((<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)
import           Text.Megaparsec                (Parsec, between, parse, some)
import           Text.Megaparsec.Char           (letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (errorBundlePretty)

type Parser = Parsec Void Text

data Field = Channel | Temperature | ID deriving (Show, Eq)

data Guess = Guess Expr Text deriving (Show, Eq)

data Expr = EComp Field Ordering Double
          | EAnd Expr Expr
          | EOr Expr Expr
          deriving (Show, Eq)

parseExpr :: Parser Expr
parseExpr = makeExprParser inner operators

  where
    operators = [[blog "&&" EAnd, blog "||" EOr]]
    blog s c = InfixL (c <$ symbol s)

    inner = between (symbol "(")  (symbol ")") parseExpr <|> fragment

    fragment = EComp <$> parseField <*> parseComp <*> lexeme (L.signed sc L.decimal)

    parseComp = EQ <$ symbol "="
                <|> LT <$ symbol "<"
                <|> GT <$ symbol ">"

    parseField = Channel <$ symbol "channel"
                 <|> Temperature <$ symbol "temperature"
                 <|> ID <$ symbol "id"

parseGuess :: Parser Guess
parseGuess = Guess <$> lexeme parseExpr <* symbol "->" <*> lexeme label

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

label :: Parser Text
label = pack <$> some letterChar

parseFile :: Parser a -> String -> IO a
parseFile f s = readFile s >>= (either (fail . errorBundlePretty) pure . parse f s) . pack

parseConfFile :: String -> IO [Guess]
parseConfFile = parseFile (sc >> some parseGuess)
