{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures #-}
import Safe (atMay)

data Type = Arrow Type Type
          | Boolean

data TypeWithVariables = VArrow TypeWithVariables TypeWithVariables
                       | VBoolean
                       | TypeVar Int
                       deriving Show

data Expr = Var Int
          | Call Expr Expr
          | Lambda Expr
          | TrueExpr
          | FalseExpr
          | IfThenElse Expr Expr Expr
          deriving Show

data LabeledExpr = LVar TypeVariable Int
                 | LCall TypeVariable LabeledExpr LabeledExpr
                 -- Lambdas store the type var for the arg too
                 -- (As the second TypeVariable)
                 | LLambda TypeVariable TypeVariable LabeledExpr
                 | LTrueExpr TypeVariable
                 | LFalseExpr TypeVariable
                 | LIfThenElse TypeVariable LabeledExpr LabeledExpr LabeledExpr
                 deriving Show

type TypeVariable = Int

-- In this situation all the constraints we need to generate
-- are of the form typeVariable = typeWithVariables
-- This makes solving them more straightforward
type TypeEquation = (TypeVariable, TypeWithVariables)

labelExpr e = fmap fst $ labelExpr' e [] (-1) 

labelExpr' :: Expr -> [TypeVariable] -> Int -> Maybe (LabeledExpr, Int)
labelExpr' TrueExpr _ id = Just (LTrueExpr (id+1), id+1)
labelExpr' FalseExpr _ id = Just (LFalseExpr (id+1), id+1)
labelExpr' (Var index) bound id = do
  typeVariable <- atMay bound index
  return $ (LVar typeVariable index, id)

labelExpr' (Call f x) bound id = do
  (labeledF, id') <- labelExpr' f bound id
  (labeledX, id'') <- labelExpr' x bound id'
  return (LCall (id''+1) labeledF labeledX, id''+1)

labelExpr' (Lambda body) bound id = do
  (labeledBody, id') <- labelExpr' body (id+1:bound) (id+1)
  return (LLambda (id'+1) (id+1) labeledBody, id'+1)

labelExpr' (IfThenElse bool ifTrue ifFalse) bound id = do
  (labeledB, id'  ) <- labelExpr' bool    bound id
  (labeledT, id'' ) <- labelExpr' ifTrue  bound id'
  (labeledF, id''') <- labelExpr' ifFalse bound id''
  return (LIfThenElse (id'''+1) labeledB labeledT labeledF, id'''+1)

generateConstraints :: LabeledExpr -> [TypeEquation]
generateConstraints (LTrueExpr t) = [(t, VBoolean)]
generateConstraints (LFalseExpr t) = [(t, VBoolean)]
generateConstraints (LVar _ _) = []
generateConstraints (LCall t f x) =
  generateConstraints f ++ generateConstraints x ++
  [(getTypeVar f, VArrow (TypeVar $ getTypeVar x) (TypeVar t))]
generateConstraints (LLambda t argT body) =
  generateConstraints body ++
  [(t, VArrow (TypeVar argT) (TypeVar $ getTypeVar body))]
generateConstraints (LIfThenElse t bool ifTrue ifFalse) =
  generateConstraints bool ++ generateConstraints ifTrue
  ++ generateConstraints ifFalse ++
  [ (getTypeVar bool, VBoolean)
  , (t, TypeVar $ getTypeVar ifTrue)
  , (t, TypeVar $ getTypeVar ifFalse)
  ]


firstTwoElements :: (a, b, c) -> (a, b)
firstTwoElements (a, b, _) = (a, b)

getTypeVar :: LabeledExpr -> TypeVariable
getTypeVar (LTrueExpr t) = t
getTypeVar (LFalseExpr t) = t
getTypeVar (LVar t _) = t
getTypeVar (LCall t _ _) = t
getTypeVar (LLambda t _ _) = t
getTypeVar (LIfThenElse t _ _ _) = t

c = Call (Lambda (Var 0)) (Var 0)
Just (labeled, x) = labelExpr' c [0] 0
talkExample = Lambda $
                IfThenElse (Call (Var 0) TrueExpr)
                  FalseExpr
                  TrueExpr

main = print $ generateConstraints labeled
