module Parser where
    
import Data.String
import Data.List
import Text.Read
import Control.Monad

-- rekurzívne sa vola checkRules na všetky pravidlá v množine
checkRecursiveRules :: [[String]] -> [String] -> [String] -> [[String]]
checkRecursiveRules rules states alpha
    | rules == [] = []
    | otherwise = (checkRules (head rules) states alpha ) : checkRecursiveRules (tail rules) states alpha

-- kontrola, či pravidlo je v správnom tvare
checkRules :: [String] -> [String] ->  [String] -> [String]
checkRules rules states alpha -- najprv je kontrola či prvý stav je v množine stavov, ak áno kontroluje sa 2. prvok pravilda
    | rules == [] = []
    | rules == [""] = []
    | elem (deleteFirstNSpace(head rules)) states == True = deleteFirstNSpace(head rules) : (compareAlpha (tail rules) states alpha) 
    | otherwise = error "Zle pravidla" 
        where 
            compareAlpha rules states alpha -- kontrola, či 2. prvok je z danej abecedy
                | rules == [] = error "Zla abeceda v pravidle"
                | elem (deleteFirstNSpace(head rules)) alpha == True = deleteFirstNSpace(head rules) : (compareRule (tail rules) states alpha)  
                | otherwise = error "Zla abeceda v pravidle"
                    where
                        compareRule rules states alpha -- kontrola či posledný prvok je z množiny stavov, ak nie je to chyba 
                            | rules == [] = error "Chyba posledny stav"
                            | elem (deleteFirstNSpace(head rules)) states == True = deleteFirstNSpace(head rules) : (tail rules)  
                            | otherwise = error "Chyba posledny stav"


-- kontrola, či stavy obsahujú iba čísla
checkStates :: [String] -> [String]
checkStates states
    | states == [] = []
    | states == [""] = []
    | (readMaybe (head states) :: Maybe Int) == Nothing = error "Chyba v stavoch"
    | otherwise = deleteFirstNSpace(head states) : (checkStates (tail states))

-- odstráni čiarky a rozdelí retazec na množinu retazcov na základe čiarky
split :: String -> [String]
split [] = [""]
split (c:cs)| c == ','  = "" : rest
            | c == '\r' = "" : rest
            | otherwise = (c : head rest) : tail rest
    where rest = split cs


-- split aplikovaný ka každý riadok monžiny reťazcov
recursiveSplit :: [String] -> [[String]]
recursiveSplit list
    | list == [] = []
    | otherwise = split (head list) : (recursiveSplit (tail list))


-- kontrola, či daná abeceda onsahuje iba znaky od a po z 
checkAlpha :: [Char] -> [String]
checkAlpha alpha
    | alpha == [] = []
    | alpha == ['\r'] = []
    | elem (head alpha) ['a' .. 'z'] == True = [(head alpha)] : checkAlpha (tail alpha)
    | otherwise = error "Chyba v abecete"


-- kontrola či stav je v abecede
checkFirstState :: String -> [String] -> String
checkFirstState firstState states
    | (elem firstState states) == True = firstState
    | otherwise = error "Pociatocny stav nieje v abecede"


-- ostráni z retazca medzery na začiatku a konci
deleteFirstNSpace :: String -> String
deleteFirstNSpace str = reverse (deleteFirstN (reverse (deleteFirstN str)))
    where
        deleteFirstN str
            | str == [] = []
            | (head str) == ' ' = deleteFirstN (tail str)
            | (head str) == '\r' = deleteFirstN (tail str)
            | otherwise = str


-- kontrola či koncové stavy sú v množine statov, vracia sa True ak áno, False ak nie
checkFinalStates :: [String] -> [String] -> Bool
checkFinalStates finalStates states
    | finalStates == [] = True
    | (elem (head finalStates) states) == True = checkFinalStates (tail finalStates) states
    | otherwise = False


-- odstráni rovnaké symboly
deleteDuplicates :: [String] -> [String]
deleteDuplicates rules
    | rules == [] = []
    | elem (head rules) (tail rules) == True = deleteDuplicates (tail rules)
    | otherwise = (head rules) : deleteDuplicates (tail rules)


-- skontroluje syntax všetkých riadkov z listu, a vrati -> stavy, abecedu, prvý stav, koncové stavy a pravidlá 
checkSyntax ::Monad m => [String] -> m ([String], [String], String, [String], [[String]])
checkSyntax list = do
    when ( (length list) < 4 ) $ error "Nespravny vstup" -- list musí mať aspon 4 riadky -> stavy, abecedum prvý stav a koncové stavy
    
    let firstLine = (head list)
    let secondLine = (head (tail list))
    let thirdLine = deleteFirstNSpace (head (tail (tail list)))
    let fourthLine = (head (tail (tail (tail list))))
    let rulesLine = (tail (tail (tail (tail list))))

    let states = sort (deleteDuplicates (checkStates (split firstLine)))
    let alpha  = sort (deleteDuplicates (checkAlpha secondLine))
    let firstState = (checkFirstState thirdLine states)
    let finalStates = sort (deleteDuplicates (checkStates (split fourthLine)))

    let rules = sort (checkRecursiveRules (recursiveSplit rulesLine) states alpha)

    when ( checkFinalStates finalStates states == False ) $ error "Nespravne koncove stavy"
    when (firstState == []) $ error "Musí byť zadaný počiatočný stav"
    when (finalStates == []) $ error "Koncové stavy nesmú byť prázdne"

    return $ (states, alpha, firstState, finalStates, rules)