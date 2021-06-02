{-# OPTIONS_GHC -Wall #-}
module IntgExpr where

import Text.ParserCombinators.Parsec

data Expr = Const Integer 
          | BinOp Op Expr Expr 
             deriving (Show, Eq)
data Op   = Plus | Minus | Times | Div | Mod  
             deriving (Show, Eq)

------------------------------
eExpr :: Expr -> Integer 
eExpr (Const v)        =  v 
eExpr (BinOp bo e1 e2) = applyBo bo (eExpr e1) (eExpr e2) 

applyBo :: Op -> Integer -> Integer -> Integer 
applyBo Plus v1 v2  = v1 + v2  
applyBo Minus v1 v2 = v1 - v2
applyBo Times v1 v2 = v1 * v2
applyBo Div v1 v2   = if v2 /= 0 then div v1 v2 else error "DivOnZero"
applyBo Mod v1 v2   = if v2 /= 0 then mod v1 v2 else error "ModOnZero"

---------------------------------
decimal :: Parser Integer
decimal = do ds  <- many1 digit
             return (read ds) 

lexem :: Parser a -> Parser a
lexem p = do a <- p
             spaces
             return a       

symbol :: String ->  Parser ()
symbol st = lexem $ do _ <- string st
                       return ()

oper  :: String -> Op -> Parser (Expr -> Expr -> Expr)
oper str bop = do symbol str
                  return $ BinOp bop 

mulOp, addOp :: Parser (Expr -> Expr -> Expr)   
mulOp = (oper "*" Times) <|> (oper "/" Div) <|> (oper "%" Mod)
addOp = (oper "+" Plus) <|> (oper "-" Minus)

parens :: Parser a -> Parser a
parens p = do symbol "("
              e <- p
              symbol ")"
              return e 

factor, term, expr, full :: Parser Expr
factor  = parens expr 
       <|> do v <- lexem decimal
              return (Const v)
--term = chainl1 factor $ do mo <- mulOp
--                           return (BinOp mo)   
--expr = chainl1 term $ do ao <- addOp 
--                         return (BinOp ao)  			  
term = chainl1 factor mulOp   
expr = chainl1 term addOp  
full = do spaces
          v <- expr
          eof
          return v

parseExpr :: String -> Expr
parseExpr s = case parse full "" s of
                 Left _  -> error "Syntax"
                 Right e -> e

ex1 :: Expr 
ex1 = BinOp Plus (Const 5) (BinOp Times (Const 4) (Const 3))

ex1Str :: String 
ex1Str = "  5 + 4   * 3 " 

---------------------------------
{-
decimal :: Parser Integer
decimal = (many1 digit) >>= (return . read)   

lexem :: Parser a -> Parser a
lexem p = p <* spaces  

symbol :: String ->  Parser ()
symbol st = lexem (string st >> return ())

oper  :: String -> Op -> Parser Op
oper str bop = symbol str >> return bop 

mulOp, addOp :: Parser Op   
mulOp = (oper "*" Times) <|> (oper "/" Div) <|> (oper "%" Mod)
addOp = (oper "+" Plus) <|> (oper "-" Minus)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")" 

factor, term, expr :: Parser Expr
factor = parens expr <|> (lexem decimal >>= return . Const) <?> "factor"
term = factor `chainl1` (BinOp <$> mulOp )   -- (expOp mulOp)     
expr = term `chainl1` (BinOp <$> addOp)  -- (expOp addOp)

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (spaces *> expr <* eof) "" s
-}
{-
expr    =  term , {addOp , term} ;
addOp   =  ‘+’ | ‘_’ ;
term    =  factor , {mulOp , factor} ;
mulOp   =  ‘*’ | ‘/’ | ‘%’ ;
factor  =  decimal | ‘(’ , expr , ‘)’ ;
decimal = digit , {digit} ;
digit   = ‘0’ | ‘1’ | ‘2’ | ‘3’ | ‘4’ | ‘5’ | ‘6’ | ‘7’ | ‘8’ | ‘9’ ; 
-}