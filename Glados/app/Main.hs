module Main (main) where
import System.Environment
import System.Exit
import System.IO
import Control.Applicative

data SExpr = Symbol String
        | Integer Int
        | Float Double
        | Lists [SExpr]
        | If SExpr SExpr SExpr
        deriving Show

data Ast = Call String [Ast]
        | Define String Ast
        | Symbol_Ast String
        | Boolean_Ast Bool
        | Integer_Ast Int
        | Float_Ast Double
        | Lists_Ast [Ast]
        | Lambda [String] Ast Ast
        | Procedure Ast Ast
        deriving Show

data Error = Error String
        deriving Show

instance Eq Ast where
    (Symbol_Ast s1) == (Symbol_Ast s2) = s1 == s2
    (Boolean_Ast b1) == (Boolean_Ast b2) = b1 == b2
    _ == _ = False

-- A CHANGER LE NOM DE LA FONCTION, MAIS ça FONCTIONNE

test2 :: Ast -> String
test2 (Lists_Ast []) = ""
test2 (Lists_Ast (Symbol_Ast h : u)) = h ++ " " ++ test2 (Lists_Ast u)
test2 (Lists_Ast (Integer_Ast h : u)) = show h ++ " " ++ test2 (Lists_Ast u)
test2 (Lists_Ast (Lists_Ast h : u)) = "(" ++ test2 (Lists_Ast h) ++ ")" ++ " " ++ test2 (Lists_Ast u)
test2 (Lists_Ast (Define s ast :u)) = "(define " ++ s ++ " " ++ test2 ast ++ ")" ++ " " ++ test2 (Lists_Ast u)
test2 (Lists_Ast (Define s ast : [])) = "(define " ++ s ++ " " ++ test2 ast ++ ")"
test2 (Symbol_Ast s) = s
test2 _ = ""

test :: Ast -> String
test (Procedure a b) = "((lambda (" ++ (test2 a) ++ ") (" ++ (test2 b) ++ "))"
test _ = ""

-- FIN DE LA FONCTION A CHANGER

defineVar :: Ast -> IO ()
defineVar (Define s (Procedure args expr)) = setEnv s (test (Procedure args expr))
defineVar (Define s (Integer_Ast i)) = setEnv s (show i)
defineVar (Define s (Symbol_Ast str)) = setEnv s str
defineVar (Define s value) = setEnv s (show value)
defineVar _ = putStrLn "Invalid define AST"

lengthOfValargs :: Ast -> Int
lengthOfValargs (Lists_Ast lst) = length lst
lengthOfValargs _ = 0

newSetEnv :: [String] -> Ast -> IO ()
newSetEnv (var:next) (Lists_Ast (Integer_Ast a:b)) = setEnv var (show a) >> newSetEnv next (Lists_Ast b)
newSetEnv (var:next) (Lists_Ast (Symbol_Ast a:b)) = setEnv var a >> newSetEnv next (Lists_Ast b)
newSetEnv (var:next) (Lists_Ast (Boolean_Ast a:b)) = setEnv var (show a) >> newSetEnv next (Lists_Ast b)
newSetEnv [] _ = return ()
newSetEnv _ _ = return ()

lookEnv :: Ast -> IO (Maybe Ast)
lookEnv (Symbol_Ast s) = do
    value <- lookupEnv s
    case value of
        Nothing -> return Nothing
        Just strValue -> case reads strValue of
            [(intValue, "")] -> return $ Just (Integer_Ast intValue)
            _ -> return $ Just (Symbol_Ast strValue)
lookEnv _ = return $ Just (Symbol_Ast "Error in lookEnv, not a Symbol_Ast")

-- creation de fonction juste pour la lambda

concatStrWithAst :: String -> Ast -> String
concatStrWithAst str (Lists_Ast lst) = str ++ " " ++ unwords (map astToString lst)
concatStrWithAst str (Symbol_Ast s) = str ++ " " ++ s
concatStrWithAst str (Integer_Ast s) = str ++ " " ++ show s
concatStrWithAst str (Boolean_Ast s) = str ++ " " ++ show s
concatStrWithAst str (Define s ast) = str ++ " " ++ s
concatStrWithAst str _ = str

pgcd :: Int -> Int -> Int
pgcd a 0 = a
pgcd a b = pgcd b (a `mod` b)

withoutListsAST :: Ast -> Ast
withoutListsAST (Lists_Ast []) = Lists_Ast []
withoutListsAST (Lists_Ast (Symbol_Ast h : u)) = Lists_Ast (Symbol_Ast h : u)
withoutListsAST (Lists_Ast (Integer_Ast h : u)) = Lists_Ast (Integer_Ast h : u)
withoutListsAST (Lists_Ast (Lists_Ast h : u)) = Lists_Ast (Lists_Ast h : u)
withoutListsAST _ = Lists_Ast []

browseList_Ast :: Ast -> IO (Maybe Ast)
browseList_Ast (Lists_Ast []) = return $ Just (Lists_Ast [])
browseList_Ast (Lists_Ast(l:rest)) = do
    Just result <- evalAST l
    Just restResults <- browseList_Ast (Lists_Ast rest)
    case restResults of
        Lists_Ast [] -> return $ Just (Lists_Ast [result])
        Lists_Ast [result2] -> return $ Just (Lists_Ast [result, result2])
        Lists_Ast (result2:rest) -> return $ Just (Lists_Ast (result:result2:rest))
        _ -> return $ Just (Lists_Ast[result , (withoutListsAST restResults)])

changeInAllName :: String -> Ast -> Ast
changeInAllName s (Lists_Ast lst) = Lists_Ast (map (changeInAllName s) lst)
changeInAllName s (Symbol_Ast str) = Symbol_Ast (s ++ str)

changeInAstWithAst2 :: String -> Ast -> Ast -> Ast
changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest)) (Lists_Ast expr) = Lists_Ast (map (changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest))) expr)
changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest)) (Symbol_Ast s)
    | sa == s = Symbol_Ast (str ++ s)
    | otherwise = Symbol_Ast s
changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest)) (Define s ast) = Define s (changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest)) ast)
changeInAstWithAst2 str a b = b


changeInAstWithAst :: String -> Ast -> Ast -> Ast
changeInAstWithAst str (Lists_Ast []) expr = expr
changeInAstWithAst str (Lists_Ast (Symbol_Ast sa:rest)) expr = do
    let newName = changeInAstWithAst2 str (Lists_Ast (Symbol_Ast sa:rest)) expr
    changeInAstWithAst str (Lists_Ast rest) newName

changeNameArgs :: String -> Ast -> Ast
changeNameArgs s (Procedure args expr) = Procedure (changeInAllName s args) (changeInAstWithAst s args expr)
changeNameArgs s a = a


evalAST :: Ast -> IO (Maybe Ast)
evalAST (Lambda args expr valargs)
    | (length args) == (lengthOfValargs valargs) = do
        newArgs <- browseList_Ast valargs
        case newArgs of
            Just (Lists_Ast newArgs) -> do
                newSetEnv args (Lists_Ast newArgs)
                evalAST expr
            _ -> return $ Just (Symbol_Ast "Error in Lambda, the valargs are not a valid Ast")
    | otherwise = return $ Just (Symbol_Ast "Error in Lambda, the 2 tabs have not the same lenght")
evalAST (Lists_Ast(Define str value: [])) = do
    result <- evalAST value
    case result of
        Just result' -> do
            defineVar (Define str result')
            return $ Just (Symbol_Ast "DEFINED")
        Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the value of define are not a valid Ast")
evalAST (Define s (Symbol_Ast args)) = do
    newArgs <- evalAST (Symbol_Ast args)
    case newArgs of
        Just newArgs' -> do
            defineVar (Define s newArgs')
            return $ Just (Symbol_Ast "DEFINED")
        Nothing -> return $ Just (Symbol_Ast "Error in Define, evalAst return Nothing")
evalAST (Define s args) = do
    -- print ("s : " ++ show s)
    -- print ("args : " ++ show args)
    let lala = (changeNameArgs s args)
    -- print ("lala : " ++ show lala)
    resultAst <- evalAST lala
    case resultAst of
        Just args' -> do
            defineVar (Define s args')
            return $ Just (Symbol_Ast "DEFINED")
        Nothing -> return $ Just (Symbol_Ast "Error in Define, evalAst return Nothing")
evalAST (Symbol_Ast "#f") = return $ Just (Boolean_Ast False)
evalAST (Symbol_Ast "#t") = return $ Just (Boolean_Ast True)
evalAST (Float_Ast f) = return $ Just (Float_Ast f)
evalAST (Integer_Ast i) = return $ Just (Integer_Ast i)
evalAST (Lists_Ast (Symbol_Ast "#builtin=":args)) = evalBinaryComp (==) args
evalAST (Lists_Ast (Symbol_Ast "#builtin!=":args)) = evalBinaryComp (/=) args
evalAST (Lists_Ast (Symbol_Ast "#builtin<":args)) = evalBinaryComp (<) args
evalAST (Lists_Ast (Symbol_Ast "#builtin>":args)) = evalBinaryComp (>) args
evalAST (Lists_Ast (Symbol_Ast "#builtin+":args)) = evalBinaryOp (+) args
evalAST (Lists_Ast (Symbol_Ast "#builtin-":args)) = evalBinaryOp (-) args
evalAST (Lists_Ast (Symbol_Ast "#builtin*":args)) = evalBinaryOp (*) args
evalAST (Lists_Ast (Symbol_Ast "#builtin/":args)) = evalBinaryOp div args
evalAST (Lists_Ast (Symbol_Ast "#builtin%":args)) = evalBinaryOp mod args
evalAST (Lists_Ast (Symbol_Ast "#builtin^" : args)) = evalBinaryOp (^) args
evalAST (Lists_Ast (Symbol_Ast "#builtinpgcd" : args)) = evalBinaryOp (pgcd) args
evalAST (Lists_Ast (Symbol_Ast "#builtinand" : a :b : rest))
    | null rest = evalAndOp a b
    |otherwise = return $ Just (Symbol_Ast "Error in and, too many args")
evalAST (Lists_Ast (Symbol_Ast "#builtinor" : a : b : rest))
    | null rest = evalOrOp a b
    | otherwise = return $ Just (Symbol_Ast "Error in or, too many args")
evalAST (Lists_Ast (Symbol_Ast "#builtinsqrt" : args)) = do
    resultArgs <- evalAST (Lists_Ast args)
    case resultArgs of
        Just (Integer_Ast resultArgs) | resultArgs >= 0 -> do
            let result = floor (sqrt (fromIntegral resultArgs))
            return $ Just (Integer_Ast result)
        _ -> return $ Just (Symbol_Ast "Error in sqrt, not an Integer")
evalAST (Lists_Ast[Symbol_Ast "if", testExpr, thenExpr, elseExpr]) = do
    resultAst <- evalAST testExpr
    case (resultAst) of
        Just testExpr' -> do
            if isTrue testExpr'
                then do
                    resultAst2 <- evalAST thenExpr
                    case (resultAst2) of
                        Just thenExpr' -> return $ Just thenExpr'
                        Nothing -> return $ Just (Symbol_Ast "Error in if, the then condition return Nothing, wrong parameter")
                else do
                    resultAst3 <- evalAST elseExpr
                    case (resultAst3) of
                        Just elseExpr' -> return $ Just elseExpr'
                        Nothing -> return $ Just (Symbol_Ast "Error in if, the else condition return Nothing, wrong parameter")
        Nothing -> return $ Just (Symbol_Ast "Error in if, the test condition return Nothing, wrong parameter")
evalAST (Lists_Ast (Lists_Ast l:rest)) = browseList_Ast (Lists_Ast (Lists_Ast l:rest))
-- test tkt ça marche
evalAST (Symbol_Ast s) = do
    resEnv <- lookEnv (Symbol_Ast s)
    case resEnv of
        Just result -> return $ Just (result)
        Nothing -> return $ Just (Symbol_Ast s)
evalAST (Lists_Ast (Symbol_Ast func:args)) = do
    result <- lookupEnv func
    case result of
        Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the function is not in env")
        Just val -> do
            newArgs <- evalAST (Lists_Ast args)
            case newArgs of
                Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the args are not a valid Ast")
                Just (newArgs) -> do
                    let newStr = concatStrWithAst (val) newArgs ++ ")"
                    let expr = tabToSexprTab2 newStr
                    case expr of
                        Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the expr are not a valid Ast")
                        Just (expr) -> do
                            newAst <- sexprToAST expr
                            case newAst of
                                Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the newAst are not a valid Ast")
                                Just (Integer_Ast i) -> do
                                    case newArgs of
                                        Lists_Ast [] -> return $ Just (Integer_Ast i)
                                        Lists_Ast [Integer_Ast i2] -> return $ Just (Lists_Ast [Integer_Ast i, Integer_Ast i2])
                                        Lists_Ast (Integer_Ast i2:rest) -> return $ Just (Lists_Ast (Integer_Ast i:Integer_Ast i2:rest))
                                        _ -> return $ Just (Lists_Ast [Integer_Ast i, newArgs])
                                Just (Symbol_Ast s) -> do
                                    case newArgs of
                                        Lists_Ast [] -> return $ Just (Symbol_Ast s)
                                        _ -> return $ Just (Lists_Ast [Symbol_Ast s, newArgs])
                                Just (newAst) -> do
                                    result <- evalAST newAst
                                    case result of
                                        Nothing -> return $ Just (Symbol_Ast "Error in Lists_Ast, the result of evalAST is Nothing")
                                        Just (result) -> return $ Just (result)
evalAST ast = do
    return $ Just ast

isTrue :: Ast -> Bool
isTrue (Symbol_Ast "#f") = False
isTrue (Boolean_Ast False) = False
isTrue (Symbol_Ast "#t") = True
isTrue (Boolean_Ast True) = True
isTrue _ = False

evalOrOp :: Ast -> Ast -> IO (Maybe Ast)
evalOrOp expr1 expr2 = do
    Just res1 <- evalAST expr1
    Just res2 <- evalAST expr2
    if (res1 == (Symbol_Ast "True") || res2 == (Symbol_Ast "True"))
        then return $ Just (Symbol_Ast "#t")
        else return $ Just (Symbol_Ast "#f")

evalAndOp :: Ast -> Ast -> IO (Maybe Ast)
evalAndOp expr1 expr2 = do
    Just res1 <- evalAST expr1
    Just res2 <- evalAST expr2
    if (res1 == (Symbol_Ast "True") && res2 == (Symbol_Ast "True"))
        then return $ Just (Symbol_Ast "#t")
        else return $ Just (Symbol_Ast "#f")

evalBinaryComp :: (Int -> Int -> Bool) -> [Ast] -> IO (Maybe Ast)
evalBinaryComp _ [] = return Nothing
evalBinaryComp _ [x] = return $ Just x
evalBinaryComp op (x:y:rest) = do
    a <- evalAST x
    b <- evalAST y
    case (a, b) of
        (Just (Integer_Ast a'), Just (Integer_Ast b')) -> do
            let result = a' `op` b'
            let symbol = if result then "#t" else "#f"
            evalBinaryComp op (Symbol_Ast symbol : rest)
        _ -> do
            return $ Just (Symbol_Ast "Error in evalBinaryComp, the given args are not Integer_Ast")

evalBinaryOp :: (Int -> Int -> Int) -> [Ast] -> IO (Maybe Ast)
evalBinaryOp _ [] = return Nothing
evalBinaryOp _ [x] = return $ Just x
evalBinaryOp op (x:y:rest) = do
    a <- evalAST x
    b <- evalAST y
    case (a, b) of
        (Just (Integer_Ast a'), Just (Integer_Ast b')) -> evalBinaryOp op ((Integer_Ast (a' `op` b')) : rest)
        _ -> do
            return $ Just (Symbol_Ast "Error in evalBinaryOp, the given args are not Integer_Ast")

sexpr2String :: SExpr -> Maybe [String]
sexpr2String (Lists (Symbol h : u)) = case sexpr2String (Lists u) of
    Just tailStrings -> Just (h : tailStrings)
    Nothing -> Nothing
sexpr2String _ = Just []

-- ROMAIN T BO RAPPEL TOI çA
-- ADRIEN AVAIT RAISON, QUEL BG CE MEC, IL A TOUT COMPRIS, IL EST TROP FORT

sexprToAST :: SExpr -> IO (Maybe Ast)
sexprToAST (Lists (Symbol "lambda" : args : expr : [])) = do
    exprAst <- sexprToAST expr
    case exprAst of
        Just newExpr -> do
            exprAst2 <- sexprToAST args
            case exprAst2 of
                Just newArgs -> return $ Just (Procedure newArgs newExpr)
                Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the args of lambda are not a valid Ast")
        Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of lambda are not a valid Ast")
sexprToAST (Lists ((Lists (Symbol "lambda" : args : expr : _)) : values))
    | null values = do
        exprAst <- sexprToAST expr
        case exprAst of
            Just _ -> do
                exprAst2 <- sexprToAST args
                case exprAst2 of
                    Just newArgs -> return $ Just (Procedure newArgs (Symbol_Ast ""))
                    Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the args of lambda are not a valid Ast, when no given args")
            Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of lambda are not a valid Ast, when no given args")
    | otherwise = do
        exprAst <- sexprToAST expr
        case exprAst of
            Just value -> case sexpr2String args of
                Just lstargs -> do
                    exprExpr <- sexprToAST (Lists values)
                    case exprExpr of
                        Just valargs -> return $ Just (Lambda lstargs value valargs)
                        Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of lambda are not a valid Ast, when given args")
                Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the args of lambda are not a valid Ast, when given args")
            Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of lambda are not a valid Ast, when given args")
sexprToAST (Lists (Symbol "define" : Symbol var : expr : rest))
    | null rest = do
        exprAst <- sexprToAST expr
        case exprAst of
            Just value -> return $ Just (Define var value)
            Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of define are not a valid Ast, when no given args")
    | otherwise = return $ Just (Symbol_Ast "Error in sexprToAST, the define has too many args")
sexprToAST (Lists (Symbol "if" : testExpr : thenExpr : elseExpr : [])) = do
    testAst <- sexprToAST testExpr
    thenAst <- sexprToAST thenExpr
    elseAst <- sexprToAST elseExpr
    case (testAst, thenAst, elseAst) of
        (Just testExpr', Just thenExpr', Just elseExpr') -> return $ Just (Lists_Ast[Symbol_Ast "if", testExpr', thenExpr', elseExpr'])
        _ -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of if are not a valid Ast")
sexprToAST (Lists l) = do
    args <- mapM sexprToAST l
    case sequence args of
        Just args' -> return $ Just (Lists_Ast args')
        Nothing -> return $ Just (Symbol_Ast "Error in sexprToAST, the expr of Lists are not a valid Ast")
sexprToAST (Integer i) = return $ Just (Integer_Ast i)
sexprToAST (Float i) = return $ Just (Float_Ast i)
sexprToAST (Symbol s) = return $ Just (Symbol_Ast s)
sexprToAST _ = return $ Just (Symbol_Ast "Error in sexprToAST, the expr are not a valid Ast")

astToString :: Ast -> String
astToString (Boolean_Ast True) = "#t"
astToString (Boolean_Ast False) = "#f"
astToString (Symbol_Ast "DEFINED") = ""
astToString (Symbol_Ast s) = s
astToString (Integer_Ast i) = show i
astToString (Float_Ast f) = show f
astToString (Lists_Ast lst) = "(" ++ unwords (map astToString lst) ++ ")"
astToString (Define var value) = "(Define " ++ var ++ " " ++ astToString value ++ ")"
astToString (Call str lst) = "(Call " ++ str ++ " " ++ unwords (map astToString lst) ++ ")"
astToString (Lambda args value valargs) = "(Lambda " ++ "(" ++ unwords args ++ ")" ++ " " ++ unwords (map astToString [value]) ++ " " ++ unwords (map astToString [valargs]) ++ ")"
astToString (Procedure _ _) = "#<procedure>"
astToString _ = ""

recursivite4Eval :: [SExpr] -> IO (Maybe Ast)
recursivite4Eval [] = return Nothing
recursivite4Eval (x:xs) = do
    exprAst <- sexprToAST x
    case exprAst of
        Just expr -> do
            result <- evalAST expr
            case result of
                Just result' -> do
                    case result' of
                        Symbol_Ast "DEFINED" -> recursivite4Eval xs
                        newresult -> do
                            putStrLn (astToString newresult)
                            recursivite4Eval xs
                Nothing -> recursivite4Eval xs
        Nothing -> return Nothing

-- ICI C PAUL
data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \input ->
  case runParser p1 input of
    Just (a, s) -> case runParser p2 s of
      Just (b, s2) -> Just ((a, b), s2)
      Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \input ->
  case runParser p1 input of
    Just (a, s) -> case runParser p2 s of
      Just (b, s2) -> Just (f a b, s2)
      Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
  case runParser p input of
    Nothing -> Just ([], input)
    Just (a, s) -> case runParser (parseMany p) s of
      Nothing -> Nothing
      Just (b, s2) -> Just (a:b, s2)



parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p


parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ const Nothing
parseAnyChar (c:cs) = parseOr (parseChar c) (parseAnyChar cs)

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

parseInt :: Parser Int
parseInt = (negate <$> (parseChar '-' *> parseUInt)) <|> (parseUInt)

parsePair :: Parser a -> Parser (a, a)
parsePair p = do
  parseChar '('
  a <- p
  parseChar ' '
  b <- p
  parseChar ')'
  return (a, b)

parseList :: Parser a -> Parser [a]
parseList p = Parser $ \xs -> case runParser (parseChar '(') xs of
    Nothing -> Nothing
    Just (_, s) -> case runParser (parseMany (parseAnd p (parseAnyChar [' ', ')']))) s of
        Nothing -> Nothing
        Just (a, s2) -> Just (map fst a, s2)

instance Functor Parser where
    fmap fct (Parser p) = Parser $ \input ->
        case p input of
            Nothing -> Nothing
            Just (x, rest) -> Just (fct x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input ->
        case pf input of
            Nothing -> Nothing
            Just (f, rest) ->
                case px rest of
                    Nothing -> Nothing
                    Just (x, rest2) -> Just (f x, rest2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Just (a, rest) -> Just (a, rest)
            Nothing -> p2 input

instance Monad Parser where
    return x = Parser $ \input -> Just (x, input)
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Just (a, rest) -> runParser (f a) rest
            Nothing -> Nothing

readLines :: IO [String]
readLines = do
    isEOF <- hIsEOF stdin
    if isEOF
        then return []
        else do
            line <- getLine
            rest <- readLines
            return (line : rest)

removeNewlines :: String -> String
removeNewlines = map (\c -> if c == '\n' then ' ' else c)

splitBlocks :: String -> String -> [String] -> Int -> Int -> [String]
splitBlocks [] currentBlock blocks tot com
    | null currentBlock = reverse blocks
    | otherwise = reverse (currentBlock : blocks)
splitBlocks ('$':cs) currentBlock blocks tot com = splitBlocks cs currentBlock blocks tot 1
splitBlocks (c:cs) currentBlock blocks tot com
    | c == '\n' && com == 0 = splitBlocks cs "" ([currentBlock] ++ blocks) tot 0
    | c == '\n' = splitBlocks cs currentBlock blocks tot 0
    | c == ' ' && tot == 0 && currentBlock /= [] && com == 0 = splitBlocks cs "" ([currentBlock] ++ blocks) tot com
    | c == ' ' && tot == 0 && currentBlock == [] && com == 0 = splitBlocks cs "" (blocks) tot com
    | c == '('  && com == 0 = splitBlocks cs (currentBlock ++ [c]) blocks (tot + 1) com
    | c == ')' && (tot - 1) == 0 && com == 0 = splitBlocks cs "" ([currentBlock ++ [c]] ++ blocks) (tot - 1) com
    | c == ')' && (tot - 1) /= 0 && com == 0 = splitBlocks cs (currentBlock ++ [c]) blocks (tot - 1) com
    | com == 1 = splitBlocks cs currentBlock blocks tot com
    | otherwise = splitBlocks cs (currentBlock ++ [c]) blocks tot com

tabToSexprTab :: [String] -> [SExpr]
tabToSexprTab [] = []
tabToSexprTab (x:xs) = case (runParser parseSExpr x) of
    Just (s, _) -> s : tabToSexprTab xs
    Nothing -> tabToSexprTab xs

tabToSexprTab2 :: String -> Maybe SExpr
tabToSexprTab2 a = case (runParser parseSExpr a) of
    Just (s, _) -> Just s
    Nothing -> Nothing

parseDouble :: Parser Double
parseDouble = do
  int <- parseInt
  parseChar '.'
  int_dec <- parseUInt
  let dec = fromIntegral int_dec / (10 ^ length (show int_dec))
  return (fromIntegral int + dec)

parseDoubleSexpr :: Parser SExpr
parseDoubleSexpr = Float <$> parseDouble

parseSExpr :: Parser SExpr
parseSExpr = parseDoubleSexpr <|> parseInterger <|> parseSymbol <|> parseListExpr

parseSymbol :: Parser SExpr
parseSymbol = Symbol <$> parseSome (parseAnyChar validSymbolChars)

parseInterger :: Parser SExpr
parseInterger = Integer <$> parseInt

parseListExpr :: Parser SExpr
parseListExpr = do
    parseChar '('
    expressions <- parseMany (parseSpaces *> parseSExpr <* parseSpaces)
    parseChar ')'
    return (Lists expressions)

validSymbolChars :: String
validSymbolChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$%&*+-./:<=>?@^_~#"

parseSpaces :: Parser ()
parseSpaces = () <$ parseMany (parseChar ' ')

-- FIN DE LA PARTIE DE PAUL

builtins :: IO ()
builtins = do
    setEnv "+" "((lambda (a b) (#builtin+ a b))"
    setEnv "-" "((lambda (a b) (#builtin- a b))"
    setEnv "*" "((lambda (a b) (#builtin* a b))"
    setEnv "div" "((lambda (a b) (#builtin/ a b))"
    setEnv "mod" "((lambda (a b) (#builtin% a b))"
    setEnv "sqrt" "((lambda (a) (#builtinsqrt a))"
    setEnv "pgcd" "((lambda (a b) (#builtinpgcd a b))"
    setEnv "eq?" "((lambda (a b) (#builtin= a b))"
    setEnv "<" "((lambda (a b) (#builtin< a b))"
    setEnv ">" "((lambda (a b) (#builtin> a b))"
    setEnv "neq?" "((lambda (a b) (#builtin!= a b))"
    setEnv "^" "((lambda (a b) (#builtin^ a b))"
    setEnv "and" "((lambda (a b) (#builtinand a b))"
    setEnv "or" "((lambda (a b) (#builtinor a b))"

main :: IO (Maybe Ast)
main = do
    builtins
    content <- getContents
    let content2 = removeNewlines content
    let content3 = splitBlocks content2 "" [] 0 0
    let content4 = tabToSexprTab content3
    recursivite4Eval content4
