{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
import Data.Char (isUpper)
import Data.Maybe (mapMaybe)

data Pattern = Pattern String [Pattern]

-- Name + pairs of input pattern and the condition
data Predicate = Predicate String [(Pattern, [PredicateCall])]

newtype Database = Database [Predicate]

-- Predicate name + arguments
newtype PredicateCall = PredicateCall (String, Pattern)

data QueryError = UnknownPredicate String
                  deriving Show
type Assignment = (String, String)
data AssignmentSet = Assignments [Assignment]
                   | Unsolvable
                   deriving Show
data SolutionSet = Solutions [[Assignment]]
                 | Error QueryError
                 deriving Show

instance Monoid AssignmentSet where
  mappend (Assignments []) rest = rest
  mappend (Assignments (x@(key, val):xs)) (Assignments rest) =

    case lookup key rest of
      Nothing -> mappend (Assignments xs) (Assignments $ x:rest)
      Just value ->
        if val == value then 
          mappend (Assignments xs) (Assignments rest)
        else Unsolvable

  mappend _ _ = Unsolvable

  -- An empty list of assignments means no constraints
  mempty = Assignments []

instance Monoid SolutionSet where
  mappend (Solutions l) (Solutions r) =
    Solutions $ filterSolvable $ combineAssignments <$> l <*> r
  mappend err@(Error _) _ = err
  mappend _ err@(Error _) = err

  -- Signifies a solution set where the only solutions imposes no
  -- constraints on the variables
  mempty = Solutions [[]]

-- Returns a list of groups of assignments that satisfies the query
solvePredicateCall :: Database -> PredicateCall -> SolutionSet
solvePredicateCall db (PredicateCall (pName, callPattern)) =
  let callPatternVars = getVariables callPattern
  in case dbLookup db pName of
       Nothing -> Error $ UnknownPredicate pName
       Just (Predicate _ cases) -> flattenSolutions $
         map (\caseInfo -> 
                case matchCase db callPattern caseInfo of
                  Solutions s ->
                    Solutions $ map
                      (flip rewriteAllVars callPatternVars)
                      s
                  err -> err
             ) cases
         

matchPattern :: Pattern -> Pattern -> AssignmentSet
matchPattern (Pattern nameL insideL) (Pattern nameR insideR)
  -- In the case both are vars the mapping goes from left to right
  -- That is, from the "outside scope" to the "inner scope"
  | isVariable nameL = mappend insidesMatch (Assignments [(nameL, nameR)])
  | isVariable nameR = mappend insidesMatch (Assignments [(nameR, nameL)])
  | nameL == nameR = insidesMatch
  | otherwise = Unsolvable
  where
    insidesMatch = 
      if length insideL == length insideR then
          mconcat $ fmap (uncurry matchPattern) $ zip insideL insideR
      else Unsolvable


matchCase :: Database -> Pattern -> (Pattern, [PredicateCall]) -> SolutionSet
matchCase db callPattern (casePattern, conditions) =
  let matchResult = matchPattern callPattern casePattern
  in case matchResult of
    -- No solution then
    Unsolvable -> Solutions []
    Assignments assignments ->
      mconcat $
        (Solutions [assignments]) : (fmap (solvePredicateCall db) conditions)


rewriteAllVars :: [(String, String)] -> [String] -> [(String, String)]
rewriteAllVars eqs =
  mapMaybe (\var -> getValueForVariable var eqs
                      >>= (\val -> Just (var, val)))

-- This might run forever if there is a loop in the equalities
-- like A = B; B = A;
getValueForVariable :: String -> [(String, String)] -> Maybe String
getValueForVariable var equalities = do
  -- If Nothing then no constraint in this variable
  rhs <- lookup var equalities
  if isVariable rhs then 
    getValueForVariable rhs equalities
  else
    Just rhs

flattenSolutions :: [SolutionSet] -> SolutionSet
flattenSolutions [] = Solutions []
flattenSolutions ((Solutions l):xs) =
  case flattenSolutions xs of
    Error e -> Error e
    Solutions r -> Solutions $ l ++ r
flattenSolutions ((Error e):_) = Error e


getVariables :: Pattern -> [String]
getVariables (Pattern name inside)
  | isVariable name = name : insideVars
  | otherwise = insideVars
  where insideVars = concat $ fmap getVariables inside

  
dbLookup :: Database -> String -> Maybe Predicate
dbLookup (Database []) _ = Nothing
dbLookup (Database (p@(Predicate name  _):xs)) pName
  | name == pName = Just p
  | otherwise = dbLookup (Database xs) pName


isVariable :: String -> Bool
isVariable [] = False
isVariable (x:_) = isUpper x


filterSolvable :: [AssignmentSet] -> [[Assignment]]
filterSolvable [] = []
filterSolvable (Unsolvable:xs) = filterSolvable xs
filterSolvable ((Assignments result):xs) = result:(filterSolvable xs)


combineAssignments :: [Assignment] -> [Assignment]
                       -> AssignmentSet
combineAssignments x y = mappend (Assignments x) (Assignments y)


m = Database [ Predicate "small" [( Pattern "ngtrks" [], [])]
             , Predicate "green" [( Pattern "X" []
                                  , [PredicateCall ("jumping", Pattern "X" [])]
                                  )
                                 , (Pattern "ngtrks" [], [])
                                 ]
             , Predicate "martian" [ (Pattern "X" [], [ PredicateCall ("small"  , Pattern "X" [])
                                                      , PredicateCall ("jumping", Pattern "X" [])
                                                      ]
                                     )
                                   , (Pattern "pgvdrk" [], [])
                                   ]
             , Predicate "jumping" [(Pattern "pgvdrk" [], [])]
             , Predicate "intelligent" [(Pattern "X" [],
                                          [ PredicateCall ("green"  , Pattern "X" [])
                                          , PredicateCall ("martian", Pattern "X" [])
                                          ]
                                        )
                                       ]
             ]

n = Database [Predicate "nice" [(Pattern "me" [], []), (Pattern "you" [], [])]]

