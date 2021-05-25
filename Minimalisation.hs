module Minimalisation where

import System.Environment 
import System.IO
import Control.Monad
import System.Directory
import Data.List

import PrintAutomata

-- čislo skupiny
type Tag = String

--tabulka -> kazdý typ tabulka je jedna skupina
data Table = Table {
    innerStates :: [State], -- <- stavy ktoré patria do jednej skupiny
    tableRows :: [[TableRow]]
} deriving (Eq,Ord)

--riadok tabulky obsahuje pravidlo a skupinu
data TableRow = TableRow {
    rule :: Rule,
    tag :: Tag -- číslo skupiny, podľa ktorého sa rozdelujú taublky
} deriving (Eq,Ord)


-- vyberajú sa len také pravidlá, ktoré sú dostupné, prvý stav je z množiny states
reachableRulesOnly :: [Rule] -> [State]-> [Rule]
reachableRulesOnly rules states
    | rules == [] = []
    | (elem (state1 (head rules)) states) == True = (head rules) : (reachableRulesOnly (tail rules) states)
    | otherwise = reachableRulesOnly (tail rules) states


-- vyberajú sa len také stavy, ktoré sa nachádzajú v množine newStates
isElem :: [State] -> [State] -> [State]
isElem states newStates
    | states == [] = []
    | elem (head states) newStates == True = (head states) : (isElem (tail states) newStates)
    | otherwise = (isElem (tail states) newStates)

-- odstranuje dupicitné stavy
deleteDuplicate :: [State] -> [State]
deleteDuplicate list
    | list == [] = []
    | elem (head list) (tail list) == True = deleteDuplicate (tail list)
    | otherwise = (head list) : (deleteDuplicate (tail list))

-- ponecháva iba stavy, ktoré sú v množine s0 
allReachableStates :: [State] -> [Rule] -> [State]
allReachableStates s0 rules
    | s0 == [] = []
    | rules == [] = s0
    | elem (state1 (head rules)) s0 == True = (state2 (head rules)) : (allReachableStates s0 (tail rules))
    | otherwise = (allReachableStates s0 (tail rules))

-- funkcia, ktoré odstráni nedosiahnutelne stavy
recursiveReachableStates :: [State] -> [State] -> [Rule] -> [State]
recursiveReachableStates s0 sPlus rules
    | s0 == sPlus = sPlus
    | rules == [] = []
    | otherwise = recursiveReachableStates (deleteDuplicate (allReachableStates sPlus rules)) s0 rules

-- spojí dve množiny
merge :: [a] -> [a] -> [a] 
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

--mnozina vsetkych prvých stavov z pravidiel a symbolu abecedy, ktore sa daju vytovrit z danej abecedy a stavov
allRules states alphabet = [ [x,y] | x <- states, y <- alphabet]
--mnoziva vsetkych prvých stavov z pravidiel a symbolu abecedy, ktore boli dane na vstupe
existingRules rules = [ [state1 r ,symbol r] | r <- rules]
--rozdiel predchádzajúcich množín
differenceBetweenRules states alphabet rules = allRules states alphabet \\ existingRules rules


-- vytovria sa prvidlá do sink stavu (sink stav je F)
addSinkRules :: [[String]] -> [Rule]
addSinkRules sets
    | sets == [] = []
    | otherwise = (Rule (head(head sets)) (head(tail(head sets))) "F") : addSinkRules (tail sets)


--tato funkcia odstani nedosiahnutelne stavy z automata a prida sink pravidlá
deleteNoReachableState :: Automata -> Automata
deleteNoReachableState automat = do
    let newAlpha = (alpha automat)
    let newFirstState = (firstState automat)
    let oldRules = (rules automat)
    -- spoja sa staré pravidlá spolu z novými, ktoré chýbajú a usporiadajú sa
    let newRulesComplete = sort (merge oldRules (addSinkRules (differenceBetweenRules ((states automat) ++ ["F"]) newAlpha oldRules)))
    
    let s0 = [newFirstState]
    let sPlus1 = allReachableStates s0 newRulesComplete -- <- prvý krok algoritmu
    -- vyberú sa stavy a pravidlá ktoré sú dostupné
    let newStates = sort (recursiveReachableStates s0 sPlus1 newRulesComplete)
    let newRulesReachable = sort (reachableRulesOnly newRulesComplete newStates) 
    let newFinalStates = isElem (finalStates automat) newStates
    makeAutomata newStates newAlpha newFirstState newFinalStates newRulesReachable

-----------------------
-----MINIMALIZACIA-----
-----------------------

-- POMOCNÉ Výpisy tabulky
-----------------------------------------
printTableRows rows
    | rows == [] = putStrLn  " --- "
    | otherwise = do
        printTableRow (head rows)
        printTableRows (tail rows)

printTableRow row = do
    printRule (rule row)
    putStrLn (" :" ++ (tag row) ++ ":")

printAllTable rows
    | rows == [] = putStrLn ""
    | otherwise = do
        printTableRows (head rows)
        printAllTable (tail rows)

printTable table = do
    putStrLn "----tab----"
    printArray (innerStates table)
    printAllTable (tableRows table)

printTableArray tables
    | tables == [] = putStrLn ""
    | otherwise = do
        printTable (head tables)
        printTableArray (tail tables)
-----------------------------------------

-- vráti množinu stavov, ktoré niesu koncové
noFinalStates :: [State] -> [State] -> [State]
noFinalStates states finalStates
    | states == [] = []
    | elem (head states) finalStates == True = noFinalStates (tail states) finalStates
    | otherwise = (head states) : (noFinalStates (tail states) finalStates)

makeTableRow :: Rule -> Tag -> TableRow
makeTableRow rule tag = (TableRow rule tag)

-- vytvorí jeden TableRow 
makeOne :: [Rule] -> [Alpha] -> [State] -> String -> [TableRow] 
makeOne rules alpha finalStates char
    | alpha == [] = []
    | rules == [] = []
    | (state1 (head rules)) /= char = (makeOne (tail rules) alpha finalStates char)
    | elem (state2 (head rules)) finalStates == True = (makeTableRow (head rules) "1") : (makeOne (tail rules) (tail alpha) finalStates char)
    | otherwise = (makeTableRow (head rules) "0") : (makeOne (tail rules) (tail alpha) finalStates char)

-- vráti množinu TableRow -> jedna skupina ktorá obsahuje buď všetky koncové stavy, alebo nekocové
makeListOfTableRow :: [Rule] -> [Alpha] -> [State] -> [State] -> [[TableRow]]
makeListOfTableRow rules alpha finalStates states
    | states == [] = []
    | otherwise = (makeOne rules alpha finalStates (head states)) : (makeListOfTableRow rules alpha finalStates (tail states))

makeTable states rows = (Table states rows)

-- odstráni rovnaké Tabuľky
deleteDupliciteRows :: [Table] -> [Table]
deleteDupliciteRows table
    | table == [] = []
    | elem (head table) (tail table) == True = deleteDupliciteRows (tail table)
    | otherwise = (head table) : deleteDupliciteRows (tail table)

-- odstráni rovnaké pravidlá
deleteDuplicateRules :: [Rule] -> [Rule]
deleteDuplicateRules rules
    | rules == [] = []
    | elem (head rules) (tail rules) == True = deleteDuplicateRules (tail rules)
    | otherwise = (head rules) : deleteDuplicateRules (tail rules)

-- porovná tab1 a tab2 na základe Tagu (čísla skupiny), keď sa rovnajú vráti True, keď nie False
cmpTableRow :: [TableRow] -> [TableRow] -> Bool
cmpTableRow tab1 tab2
    | tab1 == [] = True
    | tab2 == [] = True
    | (tag (head tab1)) == (tag (head tab2)) = (cmpTableRow (tail tab1) (tail tab2))
    | otherwise = False

-- zavolá cmpTableRow na celú všetky TableRow
cmpTableAll :: [[TableRow]] -> [[TableRow]] -> [[TableRow]]
cmpTableAll tables1 tables2
    | tables2 == [] = []
    | cmpTableRow (head tables1) (head tables2) == True = (head tables2) : (cmpTableAll tables1 (tail tables2))
    | otherwise = cmpTableAll tables1 (tail tables2)

-- vráti všetky stavy, ktoré patria do jednej skupiny TableRow
getStatesFromTableRow :: [[TableRow]] -> [State]
getStatesFromTableRow table
    | table == [] = []
    | otherwise = (state1( rule (head (head table)))) : getStatesFromTableRow (tail table)

-- porovná všekty skupiny Table a spojí tie, ktoré sa rovnajú
cmpTableRowRecursive :: [[TableRow]] -> [[TableRow]] -> [Table]
cmpTableRowRecursive table1 table2
    | table1 == [] = []
    | otherwise = do
        let tab = cmpTableAll table1 table2
        let states = getStatesFromTableRow tab
        (makeTable states tab) : (cmpTableRowRecursive (tail table1) table2)

-- jeden cyklus minimalizácie
makeMinimalisation :: [Table] -> [Table] -> [Table]
makeMinimalisation tables variable
    | tables == [] = variable
    | otherwise = do
        let tabRow1 = (tableRows (head tables))
        let tabRow2 = (tableRows (head tables))
        let var = merge (deleteDupliciteRows (cmpTableRowRecursive tabRow1 tabRow2)) variable
        makeMinimalisation (tail tables) var


-- vráti Tag pre skupinu
getGroupState :: [Table] -> State -> Int -> Int
getGroupState tables state variable
    | tables == [] = -1
    | elem state (innerStates (head tables)) == True = variable
    | otherwise = getGroupState (tail tables) state (variable + 1)

-- nastavý nový Tag pre skupinu 
updateOneTableRow :: [Table] -> [TableRow] -> [TableRow]
updateOneTableRow tables tableRow
    | tableRow == [] = []
    | otherwise = do
        let oldRule = (rule (head tableRow))
        let newTag = show (getGroupState tables (state2 oldRule) 0)
        let newRule = (TableRow oldRule newTag)
        newRule : updateOneTableRow tables (tail tableRow)

-- zavolá updateOneTableRow na riadok tabulky
updateTableRows :: [Table] -> [State] -> [[TableRow]] -> [[TableRow]] -> Table
updateTableRows tables states tabRows variable
    | tabRows == [] = makeTable states variable
    | otherwise = do
        let oldTableRow = head tabRows
        let newTableRow =  updateOneTableRow tables oldTableRow
        updateTableRows tables states (tail tabRows) (newTableRow : variable)


updateGroupsNames :: [Table] -> [Table]
updateGroupsNames tables = updateGroupsName tables tables

-- mení v celej tabulke Tagy
updateGroupsName :: [Table] -> [Table] -> [Table]
updateGroupsName tables1 tables2
    | tables1 == [] = []
    | otherwise = (updateTableRows tables2 (innerStates (head tables1)) (tableRows(head tables1))  [])  : updateGroupsName (tail tables1) tables2
        

-------------------------------
---Prevod Tabulky na Automat---
-------------------------------

-- vráti stavy na základe počtu tabuliek, ale v opačnom poradí [N .. 2,1]
getStatesFromTables :: [Table] -> [State]
getStatesFromTables tables
    | tables == [] = []
    | otherwise = (show (length tables -1)) : (getStatesFromTables (tail tables))

-- čísluje stavy podľa toho v akej je stav skupine, variable je pomocná premenná
getNewState :: [Table] -> State -> State -> Int -> Int
getNewState tables myState firstState variable
    | tables == [] = -1 -- toto by nemalo nikdy nastať
    | elem myState (innerStates (head tables)) == True = do
        if elem firstState (innerStates (head tables)) then
            0
        else
            variable
    | otherwise = do
        if elem firstState (innerStates (head tables)) then
            (getNewState (tail tables) myState firstState variable)
        else
            (getNewState (tail tables) myState firstState (variable+1))

-- vytvára nové pravidla z tabulky, ktoré sú očíslované
getRulesFromTable :: [Table] -> [Rule] -> State -> [Rule] 
getRulesFromTable tables rules firstState
    | rules == [] = []
    | tables == [] = []
    | otherwise = do
        let newState1 = show (getNewState tables (state1 (head rules)) firstState 1)
        let newSymbol = (symbol (head rules))
        let newState2 =  show (getNewState tables (state2 (head rules)) firstState 1)
        (Rule newState1 newSymbol newState2) : (getRulesFromTable tables (tail rules) firstState)


-- na každy koncový stav sa vola funkcia getNewState
getAllFinishStates :: [Table] -> [State] -> State -> [State]
getAllFinishStates tables finStates firstState
    | finStates == [] = []
    | otherwise = do
        let state = show (getNewState tables (head finStates) firstState 1)
        state : getAllFinishStates tables (tail finStates) firstState

-- z tabuľky a automatku DKA vytovrí nový automat MKA
tablesToAutomata :: [Table] -> Automata -> Automata
tablesToAutomata table automat = do
    let states = (reverse (getStatesFromTables table))
    let alphabet = sort (deleteDuplicate (alpha automat))
    let fStates = sort (deleteDuplicate (getAllFinishStates table (finalStates automat) (firstState automat)))
    let newRules = sort (deleteDuplicateRules (getRulesFromTable table (rules automat) (firstState automat)))
    makeAutomata states alphabet "0" fStates newRules 


-- funkcia, ktorá vracia stavy, do ktorých sa môžeme dostať zo stavu state a zároveň sa nenechádza v množine closed 
getNextState :: State -> [Rule] -> [State] -> [State]
getNextState state automataRules closed
    | automataRules == [] = []
    | elem (state2 (head automataRules)) closed == True = getNextState state (tail automataRules) closed
    | state == (state1 (head automataRules)) = do
        (state2 (head automataRules)) : getNextState state (tail automataRules) closed
    | otherwise = getNextState state (tail automataRules) closed

-- funkcia, ktorá zoraduje tabulku na základe skupín
sortTable :: [Table] -> [Table] -> [Rule] -> [State] -> [State] -> [Table]
sortTable tables1 tables2 rules open closed
    | open == [] = []
    | tables2 == [] = sortTable tables1 tables1 rules (tail open) ((head open) :closed)
    | elem (head open) ((innerStates (head tables2))) == True = do
        let opn = (tail open) ++ reverse (deleteDuplicate (reverse (getNextState (head open) rules closed)))
        (head tables2) : (sortTable tables1 tables1 rules opn ( (head open) : closed  ))
    | otherwise = sortTable tables1 (tail tables2) rules open closed

-- hlavný cyklus minimalizácie, koniec je ked nevznike už nová skupina
minimalisationRecursive :: [Table] -> [Table] -> [Table]
minimalisationRecursive table1 table2
    | (length table1) == (length table2) = table1
    | otherwise = do
        let tableMKA = updateGroupsNames (makeMinimalisation table1 [])        
        minimalisationRecursive tableMKA table1

 -- na zaciatku sa vytovrí tabulka s dvoma skupinamy rozdelených podla toho či sú koncové alebo nie
--minimalisationAutomata :: Automata -> Automata
minimalisationAutomata automat = do
    let yesFinStates = (finalStates automat)
    let noFinStates =  noFinalStates (states automat) yesFinStates
    let rowNoFinal = (sort(makeListOfTableRow (rules automat) (alpha automat) (finalStates automat) noFinStates))
    let rowFinal = (sort (makeListOfTableRow (rules automat) (alpha automat) (finalStates automat) (finalStates automat)))
    let tables  = (makeTable noFinStates rowNoFinal) : [(makeTable yesFinStates rowFinal)] -- tabulka, ktorá má 2 skupiny rozdelené podľa koncových stavov
    let tableMKA = (minimalisationRecursive tables []) -- minimalizovaná tabuľka
    let sortedTable = reverse (deleteDupliciteRows ( reverse (sortTable tableMKA tableMKA (rules automat) [(firstState automat)] [])))

    let finalAutomata = (tablesToAutomata sortedTable automat)

    if (finalStates finalAutomata) == [] then
        error "Automat sa nepodarilo zostrojiť, neexistujú žiadne koncové stavy"
    else
        finalAutomata