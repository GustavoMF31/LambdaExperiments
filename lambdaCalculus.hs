import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Safe (headDef)


instance Show Expr where
  -- Todo: Precedence rules and parenthesis
  show (Var name) = name
  show (Application f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
  show (Lambda name expr) = "\\" ++ name ++ "." ++ (show expr)  

data Expr = Var String
          | Application Expr Expr
          | Lambda String Expr
          -- Not a particularly good equality
           deriving (Eq)-- , Show)


reduce :: Expr -> Expr
-- If the var is not bound and wasn't replaced then just return it
reduce var@(Var name) = var
reduce (Application function parameter) = 
  case reduce function of  
   -- To apply a lambda reduce it's body after replacing the argumentName
   -- with the parameter
   (Lambda argumentName body) ->
      let argumentFreeVariables = getUnboundVariables parameter []
          newLambdaVarName = getUnusedName argumentFreeVariables

      -- If the name clashes with a free variable of the argument
      in if argumentName `elem` argumentFreeVariables then
--          error "Shrug"
          reduce (replaceVar
            -- Replace the clashing name
            (replaceVar body argumentName (Var newLambdaVarName))
            newLambdaVarName
            parameter)

         -- Otherwise simply reduce it with the var replaced by the
         -- parameter
         else reduce (replaceVar body argumentName parameter)

   function -> Application function (reduce parameter)

reduce (Lambda name body) =
  Lambda name (reduce body)


getUnusedName :: [String] -> String
getUnusedName usedNames = getUnusedNameHelper usedNames [[c] | c <- ['a'..'z']]
  
getUnusedNameHelper :: [String] -> [String] -> String
getUnusedNameHelper usedNames suggestions =
  let moreNames = (flip (++)) [[c] | c <- ['0'..'9']] suggestions
      possibleSuggestions = [suggestion | suggestion <- suggestions, not $ suggestion `elem` usedNames]
  in  headDef (getUnusedNameHelper usedNames moreNames) possibleSuggestions

replaceVar :: Expr -> String -> Expr -> Expr
replaceVar var@(Var name) varToReplace exprToReplaceWith
  | name == varToReplace = exprToReplaceWith
  | otherwise = var

replaceVar (Application f x) varToReplace exprToReplaceWith = 
  Application (replaceVar f varToReplace exprToReplaceWith) (replaceVar x varToReplace exprToReplaceWith)

replaceVar lambda@(Lambda lambdaVarName body) varToReplace exprToReplaceWith
  -- if the name is shadowed by the lambda do nothing
  | lambdaVarName == varToReplace = lambda
  | lambdaVarName `elem` exprToReplaceFreeVars =  
      let newLambdaVarName = getUnusedName exprToReplaceFreeVars
      in Lambda newLambdaVarName $
        replaceVar
          (replaceVar body lambdaVarName (Var newLambdaVarName))
          varToReplace
          exprToReplaceWith 


  | otherwise = Lambda lambdaVarName (replaceVar body varToReplace exprToReplaceWith)
  where exprToReplaceFreeVars = (getUnboundVariables exprToReplaceWith [])


getUnboundVariables :: Expr -> [String] -> [String]
getUnboundVariables (Var varName) alreadyBound =
  if varName `elem` alreadyBound then [] else [varName]

getUnboundVariables (Application f x) alreadyBound =
  getUnboundVariables f alreadyBound
  ++ getUnboundVariables x alreadyBound

getUnboundVariables (Lambda parameterName body) alreadyBound =
  getUnboundVariables body (parameterName : alreadyBound) 




-- Nice talk about lambda calculus
-- https://youtu.be/3VQ382QG-y4
identity = Lambda "x" (Var "x")
mockingbird = Lambda "f" (Application (Var "f") (Var "f"))
kestrel = Lambda "x" $ Lambda "y" (Var "x")
kite = call kestrel identity
cardinal = Lambda "f" $ Lambda "x" $ Lambda "y" $
            Application (Application (Var "f") (Var "y")) (Var "x")

-- Church encoding bools
t = kestrel
f = kite

-- Convenience
biapply f x y = Application (Application f x)  y
(...) = (.) (.) (.)
call = reduce ... Application

lnot = Lambda "p" $ biapply (Var "p") f t
zero = kite
one = Lambda "f" $ Lambda "x" $ Application (Var "f") (Var "x")
lsucc = Lambda "n" $ Lambda "f" $ Lambda "x" $
        Application (Var "f") (biapply (Var "n") (Var "f") (Var "x"))
