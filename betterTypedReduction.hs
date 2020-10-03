import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Safe (atMay)

data Expr = Var Int
          | Call Expr Expr
          | Lambda Expr

data FullyReducedCall = FullyReducedCall (Either Int FullyReducedCall) FullyReducedExpr
data FullyReducedExpr = FRVar Int
                      | FRCall FullyReducedCall
                      | FRLambda FullyReducedExpr

simplify = flip reduce []

reduce :: Expr -> [Maybe FullyReducedExpr] -> FullyReducedExpr
reduce (Var index) boundVariables =
  fromMaybe (FRVar index) $ join $ atMay boundVariables index
reduce (Call f x) boundVariables =
  let reducedF = reduce f boundVariables
      reducedX = reduce x boundVariables 
  in case reducedF of
    (FRLambda body) -> reduce body (Just reducedX : boundVariables)
    (FRCall call) -> FRCall $ FullyReducedCall (Right call) reducedX
    (FRVar index) -> FRCall $ FullyReducedCall (Left index) reducedX
reduce (Lambda body) boundVariables =
  FRLambda $ reduce body (Nothing : boundVariables)

frCallAsExpr :: FullyReducedCall -> Expr
frCallAsExpr (FullyReducedCall (Left i) fre) = Call (Var i) (asExpr fre)
frCallAsExpr (FullyReducedCall (Right call) fre) = Call (frCallAsExpr call) (asExpr fre)

asExpr :: FullyReducedExpr -> Expr
asExpr (FRVar i) = Var i
asExpr (FRLambda fre) = Lambda $ asExpr fre
asExpr (FRCall call) = frCallAsExpr call

