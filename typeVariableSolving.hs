{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}

import Safe (atMay)
import Data.Tuple.Extra (both)
import Data.Maybe (fromMaybe)

import Debug.Trace (trace)

data Type = Arrow Type Type
          | Boolean
          deriving Show

data Expr = Var Int
          | Call Expr Expr
          | Lambda Expr
          | TrueExpr
          | FalseExpr
          | IfThenElse Expr Expr Expr
          deriving Show

data TypeWithVariables = VArrow TypeWithVariables TypeWithVariables
                       | VBoolean
                       | TypeVar TypeVariable
                       deriving Show

type TypeEquation = (TypeWithVariables, TypeWithVariables)
type TypeVariable = Int

-- Does not return a typed tree, just constructs the
-- list of relations over the type variables
-- The Int is the amount of used up ids
-- The return Int is the new amount of used ids
typeCheck :: Expr -> Int -> [TypeVariable]
             -> Maybe (Int, TypeVariable, [TypeEquation])

typeCheck (Var index) used bound = middleMaybeUp (used, atMay bound index, [])
typeCheck (Call f x) used bound = do
  (ids, fType, fJudgements) <- typeCheck f used bound 
  (nIds, xType, xJudgements) <- typeCheck x ids bound 
  return (nIds+1, nIds+1, fJudgements ++ xJudgements
            -- f is an arrow from the type of the argument to +2
            -- ++ [ IsAnArrow fType xType (nIds+1) ]
            ++ [ (TypeVar fType, VArrow (TypeVar xType) (TypeVar $ nIds+1))] 
         )

typeCheck (Lambda body) used bound = do
  (ids, bodyType, bodyJudgements) <- typeCheck body (used+1) ((used+1) : bound)
  return (ids+1, ids+1, bodyJudgements ++
          -- ++ [IsAnArrow (ids+1) (used+1) bodyType]
          [( TypeVar (ids+1), VArrow (TypeVar $ used+1) (TypeVar bodyType) )]
         )

typeCheck TrueExpr used _ = Just (used+1, used+1,
  [(TypeVar (used+1), VBoolean)])
typeCheck FalseExpr used _ = Just (used+1, used+1,
  [(TypeVar (used+1), VBoolean)])

typeCheck (IfThenElse bool ifTrue ifFalse) used bound = do
  (ids,   bType, bJudgements) <- typeCheck bool used bound 
  (nIds,  tType, tJudgements) <- typeCheck ifTrue ids  bound 
  (nnIds, fType, fJudgements) <- typeCheck ifFalse nIds bound 
  return (nnIds+1, nnIds+1, bJudgements ++ tJudgements ++ fJudgements
            -- The condition is a bool
            -- The type of the IfThenElse is the type of the branches
            -- Consequently, the branches must have the same type
            ++ [ (TypeVar bType, VBoolean)
               , (TypeVar (nnIds+1), TypeVar tType)
               , (TypeVar (nnIds+1), TypeVar fType)
               ]
         )

-- Makes a big type equation into a list of equations on type vars
-- Takes the lhs and the rhs of the equation for convenience
unify :: TypeWithVariables -> TypeWithVariables -> Maybe [(TypeVariable, TypeWithVariables)]
-- unify (TypeVar t1) (TypeVar t2) = pure [(t1, TypeVar t2), (t2, TypeVar t1)]
unify (TypeVar t1) t2 = pure [(t1, t2)]
unify t1 (TypeVar t2) = pure [(t2, t1)]
unify VBoolean VBoolean = pure []
unify (VArrow fromL toL) (VArrow fromR toR) =
  (++) <$> unify fromL fromR <*> unify toL toR
unify _ _ = Nothing


rewriteType :: TypeVariable -> TypeWithVariables -> TypeWithVariables -> TypeWithVariables
rewriteType t1 t2 (VArrow from to) = VArrow (rewriteType t1 t2 from) 
                                            (rewriteType t1 t2 to)
rewriteType _ _ VBoolean = VBoolean
rewriteType t1 t2 (TypeVar t) = if t == t1 then t2 else TypeVar t

rewriteTypeEquation :: TypeVariable -> TypeWithVariables ->
                       TypeEquation -> TypeEquation
rewriteTypeEquation t1 t2 = both $ rewriteType t1 t2
rewriteTypeEquations t1 t2 = fmap $ rewriteTypeEquation t1 t2


solveTypeConstraints :: [(TypeVariable, TypeWithVariables)]
                        -> [TypeEquation]
                        -> Maybe [(TypeVariable, TypeWithVariables)]

solveTypeConstraints knowns eqs
  | trace ("solving " ++ show knowns ++ show eqs) False = undefined
solveTypeConstraints [] ((lhs, rhs):xs) = do
  substitutions <- unify lhs rhs
  solveTypeConstraints substitutions xs
solveTypeConstraints [] [] = Just []
solveTypeConstraints ((tVar, tValue):xs) eqs = do

  solutions <- solveTypeConstraints xs
               (rewriteTypeEquations tVar tValue eqs)

  let rewrittenTypeValue = rewriteWithSolutions tValue solutions
  return ((tVar, rewrittenTypeValue):solutions)
 

rewriteWithSolutions :: TypeWithVariables ->
                        [(TypeVariable, TypeWithVariables)]
                        -> TypeWithVariables
rewriteWithSolutions VBoolean _ = VBoolean
rewriteWithSolutions (VArrow from to) solutions =
  VArrow (rewriteWithSolutions from solutions)
         (rewriteWithSolutions to solutions)
rewriteWithSolutions (TypeVar t) solutions = 
  fromMaybe (TypeVar t) $ lookup t solutions


middleMaybeUp :: (a, Maybe b, c) -> Maybe (a, b, c)
middleMaybeUp (_, Nothing, _) = Nothing
middleMaybeUp (a, Just b, c) = Just (a, b, c)

mapSnd f = fmap (\(a, b) -> (a, f b)) 

call = Call (Lambda (Var 0)) (Var 0)
callType = typeCheck call 0 [0]
Just (_, _, equations) = callType
s = solveTypeConstraints [] equations

main = print s
