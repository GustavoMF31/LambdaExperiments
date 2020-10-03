{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

import Safe (atMay)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Data.List (elemIndex)

data Type = Arrow Type Type
          | Boolean
          | Integer
          deriving (Eq)

data Expr t lambdaRef varRef = Var t varRef
                             | Call t (Expr t lambdaRef varRef) (Expr t lambdaRef varRef)
                             | Lambda t lambdaRef (Expr t lambdaRef varRef)
                             | TrueExpr
                             | FalseExpr
                             | IfThenElse t (Expr t lambdaRef varRef)
                                            (Expr t lambdaRef varRef)
                                            (Expr t lambdaRef varRef)
                             deriving (Show, Eq)

type TypedExpr = Expr Type () Int
type UntypedExpr = Expr () () Int 

-- The minimum of type annotations required for very easy type inference
type MinimallyTyped = Expr () Type Int

-- Minimally typed but with variable names as strings
type NamedVariablesExpr = Expr () (String, Type) String

-- function : t
-- function  = \x. (...)
type Definition = (String, Type, NamedVariablesExpr)
data Program = Program { definitions :: [Definition]
                       , entryPoint :: NamedVariablesExpr 
                       }

instance Show Type where
  show (Arrow from to) = show from ++ " -> " ++ show to
  show Boolean = "Bool"
  show Integer = "Int"

--instance Show TypedExpr where
--  show (Var t index) = show index ++ " : " ++ show t
--  show (Call t f x) = "(" ++ show f ++ " " ++ show x ++ ")" ++ " : "
--                       ++ show t
--  show (Lambda t body) = "(" ++ "λ " ++ show body ++ ")" ++ " : "
--                       ++ show t
--  show (TrueExpr) = "True"
--  show (FalseExpr) = "False"
--  show (IfThenElse t bool ifTrue ifFalse) =
--    "(" ++ "if " ++ show bool ++ " then " ++ show ifTrue ++ " else "
--    ++ show ifFalse ++ ")" ++ " : " ++ show t

definitionAsLambda :: String -> Type -> NamedVariablesExpr -> NamedVariablesExpr
                      -> NamedVariablesExpr
definitionAsLambda name t definition body = 
  Call () (Lambda () (name, t) body) definition

programAsExpression :: Program -> NamedVariablesExpr
programAsExpression (Program assignments mainFunction) =
  foldl (\expr (name, t, definition) ->
          definitionAsLambda name t definition expr)
  mainFunction assignments


toVariableAsIndices :: NamedVariablesExpr -> [String] -> Maybe MinimallyTyped
toVariableAsIndices (Var _ name) bound = Var () <$> elemIndex name bound
toVariableAsIndices (Call _ f x) bound =
  Call () <$> toVariableAsIndices f bound <*> toVariableAsIndices x bound
toVariableAsIndices (Lambda () (name, t) body) bound =
  Lambda () t <$> toVariableAsIndices body (name : bound)
toVariableAsIndices (IfThenElse _ bool ifTrue ifFalse) bound =
  IfThenElse () <$> (toVariableAsIndices bool bound)
               <*> (toVariableAsIndices ifTrue bound)
               <*> (toVariableAsIndices ifFalse bound)
toVariableAsIndices TrueExpr _ = Just TrueExpr
toVariableAsIndices FalseExpr _ = Just FalseExpr


typeCheck :: TypedExpr -> [Type] -> Bool
typeCheck TrueExpr _ = True
typeCheck FalseExpr _ = True
typeCheck (Var varType index) knownVarTypes =
  case atMay knownVarTypes index of
    -- Unbound variable
    Nothing -> False
    -- Otherwise make sure it's the type we expect it to be
    Just t -> t == varType

typeCheck (Call callType f x) knownVarTypes =
     typeCheck f knownVarTypes
  && typeCheck x knownVarTypes
  && let tf = getType f
         tx = getType x
     in case tf of
        (Arrow tfa tfb) -> tfa == tx && tfb == callType
        _ -> False

typeCheck (Lambda (Arrow input _) _ body) knownVarTypes =
  typeCheck body (input : knownVarTypes)
typeCheck (Lambda _ _ _) _ = False

typeCheck (IfThenElse t bool ifTrue ifFalse) knownVarTypes = 
     -- The branches must typecheck individually
     typeCheck bool knownVarTypes
  && typeCheck ifTrue knownVarTypes
  && typeCheck ifFalse knownVarTypes
    -- The condition is a boolean
  && getType bool == Boolean
    -- The branches have the same type
  && t == getType ifTrue
  && t == getType ifFalse


getType :: TypedExpr -> Type
getType (Var t _) = t
getType (Call t _ _) = t
getType (Lambda t _ _) = t
getType (IfThenElse t _ _ _) = t
getType TrueExpr = Boolean
getType FalseExpr = Boolean

eraseTypes :: TypedExpr -> UntypedExpr
eraseTypes (Var _ index) = Var () index
eraseTypes (Call _ f x) = Call () (eraseTypes f) (eraseTypes x)
eraseTypes (Lambda _ _ body) = Lambda () () (eraseTypes body)
eraseTypes (IfThenElse _ c t f) =
  IfThenElse () (eraseTypes c) (eraseTypes t) (eraseTypes f)
eraseTypes TrueExpr = TrueExpr
eraseTypes FalseExpr = FalseExpr

data InferenceError = UnboundVariable
                    -- Type of function, type of the argument that failed
                    | WrongArgumentToFunction Type Type
                    | NotBooleanCondition
                    | BranchesWithDifferentTypes
                    deriving Show

-- checkOrRaise :: InferenceError -> Bool -> Either InferenceError TypedExpr
checkOrRaise _ True  = Right ()
checkOrRaise infError False = Left infError

-- TODO: Infer without getting rid of the names first,
-- for better error messages
infer :: MinimallyTyped -> Either InferenceError TypedExpr
infer = flip inferTypes $ [] 

inferTypes :: MinimallyTyped -> [Type] -> Either InferenceError TypedExpr
inferTypes (Var _ i) bound =
  case Var <$> atMay bound i <*> Just i of
    Nothing -> Left UnboundVariable
    Just v -> Right v
inferTypes (Call _ f x) bound = do
  typedF <- inferTypes f bound
  let functionType@(Arrow from to) = getType typedF
  typedX <- inferTypes x bound 
  let tx = getType typedX
  checkOrRaise (WrongArgumentToFunction functionType tx) $ from == tx
  return $ Call to typedF typedX

inferTypes (Lambda _ fromType body) bound = do
  typedBody <- inferTypes body (fromType : bound)
  let bodyType = getType typedBody
  return $ Lambda (Arrow fromType bodyType) () typedBody

inferTypes TrueExpr _ = Right TrueExpr
inferTypes FalseExpr _ = Right FalseExpr

inferTypes (IfThenElse _ bool ifTrue ifFalse) bound = do
  typedCondition <- inferTypes bool bound
  tIfTrue <- inferTypes ifTrue bound
  tIfFalse <- inferTypes ifFalse bound
  let returnType = getType tIfTrue
  checkOrRaise NotBooleanCondition $ getType typedCondition == Boolean
  checkOrRaise BranchesWithDifferentTypes $ returnType == getType tIfFalse
  return $ IfThenElse returnType typedCondition tIfTrue tIfFalse


-- Never call (except when testing with the repl)
runTyped :: TypedExpr -> UntypedExpr
runTyped e
  | check e = reduce (eraseTypes e) []
  | otherwise = error "Does not typecheck"


check :: TypedExpr -> Bool 
check = (flip typeCheck) []

-------- Stuff pretty much just copy and pasted from the
-------- deBruijnLambdaCalculus file
reduce :: UntypedExpr -> [Maybe UntypedExpr] -> UntypedExpr
reduce TrueExpr _ = TrueExpr
reduce FalseExpr _ = FalseExpr
reduce var@(Var _ index) boundUntypedExpressions =
    -- If the var is bound reduce it
    fromMaybe var $ join $ atMay boundUntypedExpressions index

reduce (Call _ f x) boundUntypedExpressions =
  let function = reduce f boundUntypedExpressions
      argument = reduce x boundUntypedExpressions
  in case function of
    (Lambda _ _ body) -> decreaseAllFreeVarsByOne $ reduce body (Just (increaseAllFreeVarsByOne argument) : boundUntypedExpressions)

    -- If we can't apply the function, return it with
    -- the function and the argument after being reduced
    _ -> Call () function argument

-- To reduce a lambda that's not being called
-- just reduce the body
reduce (Lambda _ _ body) boundUntypedExpressions =
    Lambda () () $ reduce body (Nothing : (fmap . fmap) increaseAllFreeVarsByOne boundUntypedExpressions)

reduce (IfThenElse _ condition ifTrue ifFalse) bound =
  let bool = reduce condition bound
  in case bool of
    TrueExpr  -> reduce ifTrue  bound
    FalseExpr -> reduce ifFalse bound
    _ -> IfThenElse () bool (reduce ifTrue bound) (reduce ifFalse bound)

increaseAllFreeVarsByOne :: UntypedExpr -> UntypedExpr
increaseAllFreeVarsByOne = increaseFreeVars 1 0

increaseFreeVars :: Int -> Int -> UntypedExpr -> UntypedExpr
increaseFreeVars amount notTouchThreshold (Var _ index) 
  | index < notTouchThreshold = Var () index
  | otherwise = Var () $ index + amount 

increaseFreeVars amount notTouchThreshold (Call _ f x) =
  Call () (increaseFreeVars amount notTouchThreshold f)
       (increaseFreeVars amount notTouchThreshold x)

increaseFreeVars amount notTouchThreshold (Lambda _ _ body) =
  Lambda () () (increaseFreeVars amount (notTouchThreshold+1) body)

increaseFreeVars _ _ TrueExpr = TrueExpr
increaseFreeVars _ _ FalseExpr = FalseExpr
increaseFreeVars amount notTouchThreshold (IfThenElse _ c t f) =
  IfThenElse () (increaseFreeVars amount notTouchThreshold c)
               (increaseFreeVars amount notTouchThreshold t)
               (increaseFreeVars amount notTouchThreshold f)

decreaseAllFreeVarsByOne :: UntypedExpr -> UntypedExpr
decreaseAllFreeVarsByOne = increaseFreeVars (-1) 0


-------- Let's code stuff with this

---- Convenience
-- P stands for program. Typing all those empty brackets is boring
pLambda = Lambda ()
pIfThenElse = IfThenElse ()
pVar = Var ()
pCall = Call ()

prog = Program [
    -- Boolean equality:
    ("booleanEquals", Arrow Boolean Boolean, 
        pLambda ("x", Boolean) $ pLambda ("y", Boolean) $
            pIfThenElse (pVar "x")
                (pIfThenElse (pVar "y")
                  TrueExpr
                  FalseExpr
                )

                (pIfThenElse (pVar "y")
                  FalseExpr
                  TrueExpr
                )

  -- And then the main function
  )] $ pCall (pVar "booleanEquals") TrueExpr

-- Do the preprocessing and run

indexed = toVariableAsIndices (programAsExpression prog) []
infered = fmap infer indexed

-- main = putStrLn $ show $ fmap runTyped $ infered
