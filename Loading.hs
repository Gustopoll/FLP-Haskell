module Loading where

import System.Environment 
import System.IO
import Control.Monad
import System.Directory

-- zo vstpuných argumentov vyberie prepínač, ktorý je zadaný, buď -i, alebo -t
getParameter :: [String] -> String
getParameter list
    | list == [] = error "Niesu zadane ziadne parametre"
    | (head list) == "-t" = getParameterEnd (tail list) "-t"
    | (head list) == "-i" = getParameterEnd (tail list) "-i"
    | otherwise = error "Nespravne zadane parametre skusk -i alebo -t"
    where
        getParameterEnd list character -- kontrola či nahodou neboli zadané viaceré paramerty
            | list == [] = character
            | (tail list) == [] = character
            | otherwise = error "Prilis vela argumentov" 


-- funkcia, ktorá načíta riadky zo súboru
getLinesFromFile :: FilePath -> IO [String]
getLinesFromFile file = do
    isMyFileExist <- doesFileExist file
    when (isMyFileExist == False) $ error "Nepodarilo sa otvorit subor"
    lines <- lines `fmap` (readFile file)
    return $ (lines)

-- funkcia, ktorá načíta riadky zo vstupu
getUserLines  :: IO [String]
getUserLines = go [""]
    where
        go contents = do
        line <- lines `fmap` getLine
        end <- isEOF
        if  end == True then
            return (contents ++ line)
        else
            go (contents ++ line)


-- odstráni zo vstupu prvý prázdny znak
removeFirstEmptyArray :: [String] -> [String]
removeFirstEmptyArray list
    | (head list) == "" = (tail list)
    | otherwise = (head list) : (tail list)


-- vráti dvojicu -> zadaný parameter (-t alebo -i) a množinu načítaných riadkov
loadData :: [String] -> IO (String, [String])
loadData args = do
    let param = getParameter args
    when ( param == []) $ error "Niesu zadane ziadne parametre" 
    let secondArgumet = (head (tail args)) 
    if (tail args) == [] then do
        lines <- getUserLines -- načítanie zo vstupu stdin
        return $ (param , removeFirstEmptyArray lines)
    else do
        lines <- getLinesFromFile secondArgumet -- načítanie zo súboru
        return $ (param , lines)
 