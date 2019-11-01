module Task8 where

import Todo(todo)

import Control.Applicative
import Control.Monad (forever)
import Data.Maybe (catMaybes)
import Data.List (find)

data Expression = 
    ExprLiteral Double 
  | ExprPlus    Expression Expression 
  | ExprMinus   Expression Expression 
  | ExprUMinus  Expression 
  | ExprMult    Expression Expression 
  | ExprDiv     Expression Expression  
  | ExprPow     Expression Expression
  | ExprFact    Expression
  deriving (Show)
  
data Operator = 
    UnaryOp  OpName (Expression -> Expression) (Double -> Double) (Double -> Bool)
  | BinaryOp OpName (Expression -> Expression -> Expression) (Double -> Double -> Double) (Double -> Double -> Bool)
  
type OpName = Char

fact, uminus, plus, minus, mult, divop, pow :: Operator
fact   = UnaryOp  '!' ExprFact   (\n-> fromIntegral $ product [1 .. floor n :: Int]) (\v -> fromIntegral (floor v :: Int) /= v)
uminus = UnaryOp  '-' ExprUMinus (\v -> -v) (const False)
plus   = BinaryOp '+' ExprPlus   (+)        (const.const False)
minus  = BinaryOp '-' ExprMinus  (-)        (const.const False)
mult   = BinaryOp '*' ExprMult   (*)        (const.const False)
divop  = BinaryOp '/' ExprDiv    (/)        (const.(/= 0.0))
pow    = BinaryOp '^' ExprPow    (**)       (\va vb -> va < 0 && vb < 0)

opName :: Operator -> OpName
opName (UnaryOp c _ _ _)  = c
opName (BinaryOp c _ _ _) = c

type Parser a = String -> Maybe (String, a)

safeHead :: [a] -> Maybe a
safeHead s = if null s then Nothing else Just $ head s

next :: Parser Char
next s = do
  h <- safeHead s
  return (tail s, h)

parseChar ::  Char -> Parser Char
parseChar c s = do
  (s', rc) <- next s
  if rc == c then Just (s', c) else Nothing
   
oneOf :: [Parser a] -> Parser a
oneOf ps s = case catMaybes $ ps <*> pure s of
  []    -> Nothing
  (h:_) -> Just h 
  
oneOfChar :: String -> Parser Char
oneOfChar ss = oneOf $ parseChar <$> ss

parseDigit :: Parser Char
parseDigit = oneOfChar "1234567890"

greedy :: Parser a -> Parser [a]
greedy p = greedy' []
  where
    greedy' acc s = case p s of
            Just (s', res) -> greedy' (acc ++ [res]) s'  
            Nothing -> Just (s, acc)

greedy1 :: Parser a -> Parser [a]
greedy1 p s = case greedy p s of
  Nothing -> Nothing
  Just (_, []) -> Nothing
  val -> val
   
parseLiteral :: Parser Expression
parseLiteral s = do
  (s', intPart) <- greedy1 parseDigit s
  case parseChar '.' s' of
    Nothing -> return (s', ExprLiteral $ read intPart) 
    Just (s'', _) -> do
      (s''', fracPart) <- greedy parseDigit s''
      return (s''', ExprLiteral $ read $ intPart ++ "." ++ fracPart)

parseParent :: Parser Expression
parseParent s = do
  (s', _) <- parseChar '(' s
  (s'', expr) <- parseExpr s'
  (s''', _) <- parseChar ')' s''
  return (s''', expr)

parseUnary :: Operator -> Parser Expression
parseUnary (BinaryOp {}) _ = undefined
parseUnary (UnaryOp c f _ _) s = do
  (s', _) <- parseChar c s
  (s'', e) <- parseExpr s'
  return (s'', f e)
  
parseTerm :: Parser Expression
parseTerm = oneOf [parseLiteral, parseUnary uminus, parseUnary fact, parseParent]

byLayer :: [[Operator]] -> Parser Expression -> Parser Expression
byLayer [] l s = l s
byLayer (ops:opss) l s = do
  (s', res) <- nextLayer s
  (s'', resTail) <- greedy layerTail s'
  return (s'', toExpr $ (head ops, res):resTail)  
  where
    opExpr (BinaryOp _ f _ _) = f
    opExpr (UnaryOp {}) = undefined
    
    nextLayer = byLayer opss l
    layerTail ts = do
      (s', opChar) <- oneOfChar (opName <$> ops) ts
      (s'', resTail) <- nextLayer s'
      op <- find ((== opChar).opName) ops
      return (s'', (op, resTail))
   
    toExpr [] = undefined
    toExpr [(_, e)] = e
    toExpr ((_, e1):(op, e2):es) = toExpr $ (op, opExpr op e1 e2) : es

parseExpr :: Parser Expression
parseExpr = byLayer [[plus, minus], [mult, divop], [pow]] parseTerm

parseInTree :: String -> Maybe Expression
parseInTree s = case parseExpr s of
  Just ([], tree) -> Just tree
  _ -> Nothing

calcUnary :: Operator -> Expression -> Maybe Double
calcUnary (BinaryOp{}) _ = undefined
calcUnary (UnaryOp _ _ op err) a = do
  aval <- calcTree a
  if err aval then Nothing else Just $ op aval
  
calcBinary :: Operator -> Expression -> Expression -> Maybe Double
calcBinary (UnaryOp{}) _ _ = undefined
calcBinary (BinaryOp _ _ op err) a b = do
  aval <- calcTree a
  bval <- calcTree b
  if err aval bval then Nothing else Just $ aval `op` bval
  
calcTree :: Expression -> Maybe Double
calcTree (ExprLiteral val) = Just val
calcTree (ExprUMinus a)  = calcUnary  uminus a
calcTree (ExprFact a)    = calcUnary  fact   a
calcTree (ExprPlus a b)  = calcBinary plus   a b
calcTree (ExprMinus a b) = calcBinary minus  a b
calcTree (ExprMult a b)  = calcBinary mult   a b
calcTree (ExprDiv a b)   = calcBinary divop  a b  
calcTree (ExprPow a b)   = calcBinary pow    a b

parse :: String -> Maybe Double
parse s = calcTree =<< parseInTree (filter (/=' ') s)

main::IO()
main = do
  putStrLn "Write expression. Ctrl-C to exit."
  forever $ do
    line <- getLine
    print $ parse line
