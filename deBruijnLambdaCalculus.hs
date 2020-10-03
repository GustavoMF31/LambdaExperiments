import Data.Maybe (fromMaybe)
import Safe (atMay)
import Control.Monad (join)

data Expr = Var Int
     | Call Expr Expr
     | Lambda Expr
     deriving (Show)

simplify :: Expr -> Expr
simplify = (flip reduce) []

reduce :: Expr -> [Maybe Expr] -> Expr
reduce var@(Var index) boundExpressions =
    -- If the var is bound reduce it
    fromMaybe var $ join $ atMay boundExpressions index

reduce (Call f x) boundExpressions =
  let function = reduce f boundExpressions
      argument = reduce x boundExpressions
  in case function of
    (Lambda body) -> decreaseAllFreeVarsByOne $ reduce body (Just (increaseAllFreeVarsByOne argument) : boundExpressions)

    -- If we can't apply the function, return it with
    -- the function and the argument after being reduced
    _ -> Call function argument

-- To reduce a lambda that's not being called
-- just reduce the body
reduce (Lambda body) boundExpressions =
    Lambda $ reduce body (Nothing : (fmap . fmap) increaseAllFreeVarsByOne boundExpressions)


decreaseAllFreeVarsByOne :: Expr -> Expr
decreaseAllFreeVarsByOne = increaseFreeVars (-1) 0

increaseAllFreeVarsByOne :: Expr -> Expr
increaseAllFreeVarsByOne = increaseFreeVars 1 0

increaseFreeVars :: Int -> Int -> Expr -> Expr
increaseFreeVars amount notTouchThreshold (Var index) 
  | index < notTouchThreshold = Var index
  | otherwise = Var $ index + amount 

increaseFreeVars amount notTouchThreshold (Call f x) =
  Call (increaseFreeVars amount notTouchThreshold f)
       (increaseFreeVars amount notTouchThreshold x)

increaseFreeVars amount notTouchThreshold (Lambda body) =
  Lambda (increaseFreeVars amount (notTouchThreshold+1) body)

-- Convenience
(...) = (.) (.) (.)

call = simplify ... Call
callAll :: [Expr] -> Expr
callAll = foldl1 Call
callAllL :: [Expr] -> Expr
callAllL = foldr1 Call
callAlls = simplify . callAll
callAllsL = simplify . callAllL

-- Lambda functions
-- prefix l stands for lambda, to prevent collisions with prelude funcs
lid = Lambda (Var 0)
kestrel = Lambda $ Lambda $ Var 1
kite = call kestrel lid

land = Lambda $ Lambda $ callAll
    [Var 1, Var 0, Var 1]
-- lor = Lambda $ Lambda $ callAll
--    [Var 1, Var 1, Var 0]
lor = mockingbird

bequals = Lambda $ Lambda $ callAll [Var 1, Var 0, call cardinal (Var 0)]

mockingbird = Lambda $ Call (Var 0) (Var 0)
cardinal = Lambda $ Lambda $ Lambda $
              callAll [(Var 2), (Var 0), (Var 1)]

-- Numbers!
zero = kite
-- one  = Lambda $ Lambda $ Call (Var 1) (Var 0)
one = lid

lsucc = Lambda $ Lambda $ Lambda $
        Call (Var 1) (callAlls [Var 2, Var 1, Var 0])

bluebird = Lambda $ Lambda $ Lambda $
            Call (Var 2) (Call (Var 1) (Var 0))

add = Lambda $ Call (Var 0) lsucc

