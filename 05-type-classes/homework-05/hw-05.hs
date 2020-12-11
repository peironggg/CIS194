{-# LANGUAGE FlexibleInstances    #-}

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M


--Exercise 1
eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add leftExpr rightExpr) = eval leftExpr + eval rightExpr
eval (Mul leftExpr rightExpr) = eval leftExpr * eval rightExpr

--Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--Exercise 3
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

--Exercise 4
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit = (0 <)
    mul = (&&)
    add = (||)

instance Expr MinMax where
    lit                                      = MinMax
    mul (MinMax firstNum) (MinMax secondNum) = MinMax $ min firstNum secondNum
    add (MinMax firstNum) (MinMax secondNum) = MinMax $ max firstNum secondNum

instance Expr Mod7 where
    lit num                              = Mod7 $ num `mod` 7
    mul (Mod7 firstNum) (Mod7 secondNum) = Mod7 $ (firstNum + secondNum) `mod` 7
    add (Mod7 firstNum) (Mod7 secondNum) = Mod7 $ (firstNum * secondNum) `mod` 7


testExp :: Expr a => Maybe a
testExp      = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

--Exercise 5
instance Expr StackVM.Program where
    lit num = [StackVM.PushI num]
    add firstProgram secondProgram = firstProgram ++ secondProgram ++ [StackVM.Add]
    mul firstProgram secondProgram = firstProgram ++ secondProgram ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

--Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
    | VarAdd VarExprT VarExprT
    | VarMul VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = VarLit
    mul = VarMul
    add = VarAdd
    
instance HasVars VarExprT where
    var = Var


instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit num = \_ -> Just num
    mul leftFunc rightFunc = \map -> case (leftFunc map, rightFunc map) of
        (Just firstVal, Just secondVal) -> Just (firstVal * secondVal)
        otherwise                       -> Nothing
    add leftFunc rightFunc = \map -> case (leftFunc map, rightFunc map) of
        (Just firstVal, Just secondVal) -> Just (firstVal + secondVal)
        otherwise                       -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


