data Expr = Var String
          | Call Expr Expr
          | Lambda String Expr
          deriving Show

-- S f g x = f x $ g x
-- K x y = x
-- I x = x
data Combinator = S | K | I deriving Show
data CombinatorExpr = Comb Combinator
                    | CombCall CombinatorExpr CombinatorExpr
                    deriving Show

data MixedCombinatorExpr = MComb Combinator
                         | MCombCall MixedCombinatorExpr MixedCombinatorExpr
                         | UnboundVar String

instance Show MixedCombinatorExpr where
  show (MComb c) = show c
  show (MCombCall f x) = "(" ++ show f ++ " " ++ show x ++ ")"
  show (UnboundVar v) = v


asCombinator :: Expr -> MixedCombinatorExpr
asCombinator (Call f x) = MCombCall (asCombinator f) (asCombinator x)
asCombinator (Var v) = UnboundVar v  
asCombinator (Lambda name body) = lambdaAsCombinator name body

lambdaAsCombinator :: String -> Expr -> MixedCombinatorExpr
lambdaAsCombinator nameToRemove (Var varName) =
  if varName == nameToRemove then
      MComb I
  else MCombCall (MComb K) (UnboundVar varName)
    
lambdaAsCombinator nameToRemove (Call f x) =
  MCombCall
    (MCombCall (MComb S) $ lambdaAsCombinator nameToRemove f)
    (lambdaAsCombinator nameToRemove x)

lambdaAsCombinator nameToRemove (Lambda varName body) =
  let combinatorBody = lambdaAsCombinator varName body
  in cleanCombinator nameToRemove combinatorBody

cleanCombinator :: String -> MixedCombinatorExpr -> MixedCombinatorExpr
cleanCombinator nameToRemove c@(MComb _) = MCombCall (MComb K) c
cleanCombinator nameToRemove (MCombCall f x) =
  MCombCall 
    (MCombCall (MComb S) $ cleanCombinator nameToRemove f)
    (cleanCombinator nameToRemove x)
  
cleanCombinator nameToRemove var@(UnboundVar varName) =
  if nameToRemove == varName then
     MComb I
  else var


-- S f g x = f x $ g x
-- K x y = x
-- I x = x
interpretCombinators :: MixedCombinatorExpr -> MixedCombinatorExpr
-- Definitions of the S K and I combinators
interpretCombinators
  (((MComb S `MCombCall` f) `MCombCall` g) `MCombCall` x) =
    interpretCombinators $ (f `MCombCall` x) `MCombCall` (g `MCombCall` x)

interpretCombinators ((MComb K `MCombCall` x) `MCombCall` y) =
  interpretCombinators x

interpretCombinators (MComb I `MCombCall` x) =
  interpretCombinators x

-- General case for calls
interpretCombinators (MCombCall f x) =
  MCombCall
    (interpretCombinators f)
    (interpretCombinators x)
interpretCombinators other = other

id = Lambda "x" $ Var "x"
c = Lambda "f" $ Lambda "x" $ Lambda "y" $
      Call (Call (Var "f") (Var "y")) (Var "x")
apC = ((c `Call` (Var "j")) `Call` (Var "fst")) `Call` (Var "snd")
combApC = asCombinator apC

main = print $ interpretCombinators combApC
