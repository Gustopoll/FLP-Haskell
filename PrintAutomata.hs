module PrintAutomata where

type State = String
type Alpha = String

data Rule = Rule {
    state1 :: State,
    symbol :: Alpha,
    state2 :: State
} deriving (Eq,Ord)

data Automata = Automata {
    states :: [State],
    alpha :: [Alpha],
    firstState :: State,
    finalStates :: [State],
    rules :: [Rule]
} deriving (Eq,Ord)

-- vypís jedného pravidla
printRule :: Rule -> IO ()
printRule rule = do
    putStr $ (state1 rule) ++ "," ++ (symbol rule) ++ "," ++ (state2 rule)

--vypíše množinu pravidel
printRules :: [Rule] -> IO ()
printRules rules
    | rules == [] = putStr  ""
    | otherwise = do
        printRule (head rules)
        putStrLn ""
        printRules (tail rules)

-- vypíše abecedu
printAlpha :: [Alpha] -> IO ()
printAlpha alpha
    | alpha == [] = putStrLn  ""
    | otherwise = do
        putStr (head alpha)
        printAlpha (tail alpha) 

-- vypíše množinu oddelenú čiarkami
printArray :: [State] -> IO ()
printArray array
    | array == [] = putStrLn  ""
    | (tail array) == [] = do
        putStr (head array)
        printArray (tail array)
    | otherwise = do
        putStr ((head array) ++ ",")
        printArray (tail array)

makeRule :: [String] -> Rule
makeRule (state1:symbol:state2:xs) = (Rule state1 symbol state2)

-- vytovrí monžinu pravildiel 
makeRules :: [[String]] -> [Rule]
makeRules rules
    | rules == [] = []
    | otherwise = (makeRule (head rules)) : makeRules (tail rules)

-- vytvorí automat 
makeAutomata :: [State] -> [Alpha] -> State -> [State] -> [Rule] -> Automata
makeAutomata states alpha firstState finalStates rules = (Automata states alpha firstState finalStates rules) 

-- vypíše automat v správnom tvare
printAutomata :: Automata -> IO ()
printAutomata automat = do
    printArray (states automat)
    printAlpha (alpha automat)
    putStrLn (firstState automat)
    printArray (finalStates automat)
    printRules (rules automat)
