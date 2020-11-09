module NameConf where

import           Control.Applicative            ((<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Char                      (isAlpha)
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)
import           Text.Megaparsec                (Parsec, parse, satisfy, some)
import           Text.Megaparsec.Char           (space1)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (errorBundlePretty)

type Parser = Parsec Void Text

data Field = Channel | Temperature | ID deriving Show

data Guess = Guess Expr Text deriving Show

data Expr = EComp Field Ordering Double
          | EAnd Expr Expr
          | EOr Expr Expr
          deriving Show

parseExpr :: Parser Expr
parseExpr = makeExprParser fragment operators

  where
    operators = [[blog "&&" EAnd, blog "||" EOr]]
    blog s c = InfixL (c <$ symbol s)

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
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

label :: Parser Text
label = pack <$> some (satisfy isAlpha)

parseFile :: Parser a -> String -> IO a
parseFile f s = readFile s >>= (either (fail . errorBundlePretty) pure . parse f s) . pack

parseConfFile :: String -> IO [Guess]
parseConfFile = parseFile (some parseGuess)
