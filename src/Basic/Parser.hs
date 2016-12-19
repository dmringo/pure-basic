{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Basic.Parser where

import Basic.AST
import Basic.Doub
  
import Data.Attoparsec.Text hiding (D)
import Data.Attoparsec.Expr
import Data.Text (Text)
import Control.Applicative
import Control.Exception
import Data.Typeable (Typeable) 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.IO
import Prelude hiding (rem, takeWhile)

import Data.String (IsString(..))

newtype ParseException = ParseException String
  deriving (Typeable)

instance Show ParseException where
  showsPrec p (ParseException msg) =
    showString $ "Syntax error:\n" ++ msg
           
instance Exception ParseException
  
parseFile :: FilePath -> IO Program 
parseFile file = do
  src <- T.readFile file
  return $ either error id
         $ parseEOF prog src

parseProgram :: Text -> Either SomeException Program
parseProgram txt = case parseEOF prog txt of
                     Left msg -> Left . toException $ ParseException msg
                     Right prg -> Right prg

               
parseEOF = parseOnly . (<* (skipSpace <* endOfInput))

opt :: Parser a -> Parser (Maybe a)
opt p = option Nothing (Just <$> p)

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r m = l *> m <* r

parens :: Parser a -> Parser a                
parens = (<?> "parens") . (sym "(" `between` sym ")")
         
-- | ignore trailing whitespace after some Parser
tok :: Parser a -> Parser a
tok = (<* takeWhile isHorizontalSpace)

-- | ignore trailing whitespace after parsing literal Text
sym :: Text -> Parser Text
sym t = tok (asciiCI t) <?> show t
      

prog :: Parser Program
prog = many' line
       
-- | Parse a single BASIC line
line :: Parser Line
line = (,) <$> lineNum <*> statements <* (endOfLine <|> endOfInput)

integral, lineNum :: Parser Int
integral = tok decimal
lineNum = integral

statements :: Parser [Stmt]
statements = tok statement `sepBy` colon

-- | Parse a literal ':' token
comma, colon, equals :: Parser Text
comma     = sym ","
colon     = sym ":"
semicolon = sym ";"
equals    = sym "="     

litString :: Parser Text
litString = tok $ singleQStr <|> doubleQStr
  where singleQStr = "'"  *> takeTill (== '\'') <* "'"
        doubleQStr = "\"" *> takeTill (== '"')  <* "\""

litNum :: Parser Doub
litNum = D <$> tok myScientific
  where myScientific = combine <$> doublish <*> (option 0 exponent)
        combine d e  = d * 10 ** e
        exponent     = "e" *> negInteg
        doublish     = dotSomething <|> somethingDot
        integ        = fromIntegral <$> decimal
        negInteg     = (*) <$> option 1 ("-" *> pure (-1)) <*> integ
        decimalize 0 = 0
        decimalize x = let e = fromIntegral . floor . logBase 10 $ x
                       in x / 10 ^ (e + 1)
                      
        -- .012, eg
        dotSomething = decimalize <$> "." **> integ
        -- 1. or 1.01, eg
        somethingDot = (+) <$> integ <*>
                       (decimalize <$> option 0 ("." *> (option 0 integ)))
                       
         
statement :: Parser Stmt
statement = tok $ choice
            [ rem, goto, ongoto, gosub, ongosub, set, for, next, condgo
            , while, wend, cond, dim, ret, end, output, input, nop]


rem, goto, ongoto, gosub, ongosub, set, for, next :: Parser Stmt
while, wend, cond, condgo, dim, ret, end, output, input :: Parser Stmt

-- | tighter binding version of (*>)
infixl 5 **> 
(**>) :: Applicative f => f a -> f b -> f b
(**>) = (*>)
        
rem     = REM     <$> sym "rem"   **> takeTill isEndOfLine
goto    = GOTO    <$> sym "goto"  **> lineNum
gosub   = GOSUB   <$> sym "gosub" **> lineNum
ongoto  = ONGOTO  <$> sym "on"    **> expr <*> sym "goto"  **> linums
ongosub = ONGOSUB <$> sym "on"    **> expr <*> sym "gosub" **> linums
linums = lineNum `sepBy1` comma          
next    = NEXT    <$> sym "next"  **> var `sepBy` comma
while   = WHILE   <$> sym "while" **> opt expr
wend    = WEND    <$> sym "wend"  **> opt expr

dim     = DIM     <$> sym "dim"   **> dimpair `sepBy1` comma
  where dimpair = (,) <$> varName <*> dims
          
set     = LET     <$> (opt $ sym "let") **> var <*> equals **> expr
for     = FOR     <$> sym "for" **> numVar <*> equals **> expr <*>
          sym "to" **> expr <*> opt (sym "step" **> expr)
condgo  = IFGO    <$> ifthen <*> lineNum <*> pure False
cond    = IF      <$> ifthen <*> statements <*> opt (sym "else" **> statements)
ifthen  = sym "if" **> expr <* sym "then"
              
nop     = pure NOP <* skipSpace
          
output   = PRINT <$> sym "print" **> many' printArg
  where printArg = choice [litTab, pcom, psem, preg]
        litTab = PTab <$> sym "tab" **> parens expr <* opt (semicolon <|> comma)
        pcom = PCom <$> expr <* comma
        psem = PSem <$> expr <* semicolon
        preg = PReg <$> expr

input    = INPUT <$> sym "input" **> optPrompt <*> invars
  where invars = stringVar `sepBy1` comma  <|> numVar `sepBy1` comma
        optPrompt = opt litString <* opt (semicolon <|> colon)
                 
end     = pure END <* sym "end"
ret     = pure RET <* sym "return"


dims :: Parser Dims
dims = Dims <$> parens (expr `sepBy1` comma)
          
var, stringVar, numVar, numArr, stringArr :: Parser Var

-- | A Var is a string variable, a numeric variable, or an index into an array.
var = choice [stringArr, numArr, stringVar, numVar]

-- | String variables have a '$' suffix      
stringVar = SVar <$> (T.snoc <$> varName <*> tok (char '$'))

-- | Non-indexed, non-'$'-suffixed variables are numeric            
numVar = NVar <$> tok varName

-- | Indexed numeric array reference
numArr = NArr <$> varName <*> dims

-- | Indexed string array reference
stringArr = SArr <$> (T.snoc <$> varName <*> tok (char '$')) <*> dims
         
-- | Parse a variable name.  Legal characters are alphanumeric and underscore.
-- First character must be a letter.  31 character limit is ignored.
varName :: Parser Text
varName = T.cons <$> letter <*> takeTill (notInClass varChars)
  where varChars = '_' : ['0'.. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

           
expr, parenexpr, funcall, variable, literal :: Parser Expr
expr = buildExpressionParser opTab term

term = choice [parenexpr, funcall, variable, literal]
       
-- | Functions are limited to the `int()` rounding function and the `rnd()`
-- function for generating random numbers.
funcall = intCall <|> rndCall
  where
    -- Only recognizing "int(x) and rnd(x)" as functions for now.
    -- A more general approach should be taken if more are added.
    intCall = FunCall <$> sym "int" <*> anArg
    rndCall = FunCall <$> sym "rnd" <*> anArg
    anArg = (:[]) <$> parens expr
            
variable = Var <$> var
literal = Lit <$> ((LStr <$> litString <|> LNum <$> litNum))
parenexpr = Paren <$> parens expr

opTab :: OperatorTable Text Expr
opTab =
  [ -- Exponentiation
    [mkInfix ("^", Pow, AssocRight)]

  , -- Unary negation and positive
    [ Prefix (sym "-" *> pure (Prim . Neg) <?> "unary negate")
    , Prefix (sym "+" *> pure id <?> "unary positive")]
  
  , -- Multiplication, division, modulus
    map mkInfix [ ("*"  , Mul, AssocLeft)
                , ("/"  , Div, AssocLeft)
                , ("mod", Mod, AssocLeft)]

  , -- Addition and subtraction
    map mkInfix [ ("+" , Add , AssocLeft)
                , ("-" , Sub , AssocLeft)]

  , -- Relational operators
    map mkInfix [ ("=" , Eq  , AssocLeft)
                , ("<" , Lt  , AssocLeft)
                , (">" , Gt  , AssocLeft)]
    
  , -- Have to be broken up into a separate tier due to
    -- internal behavior of buildExpressionTable
    map mkInfix 
                [ ("<=", Lte , AssocLeft)
                , (">=", Gte , AssocLeft)
                , ("<>", Neq , AssocLeft)]
    
  , -- Logical Not
    [Prefix (sym "not" *> pure (Prim . Not))]
    
  , -- Bitwise AND
    [mkInfix ("and", And, AssocLeft)]
    
  , -- Bitwise OR and exclusive OR
    map mkInfix [ ("or" , Or , AssocLeft)
                , ("xor", Xor, AssocLeft)]
  ]

mkInfix (opsym, op, assoc) =
  Infix (sym opsym **> pure (\l r -> Prim $ op l r)) assoc
