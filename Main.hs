
import System.Environment 
import System.IO
import Control.Monad
import System.Directory

import Loading -- loadData
import Parser -- checkSyntax
import PrintAutomata -- printAutomata, makeAutomata, makeRules
import Minimalisation -- minimalisationAutomata, deleteNoReachableState

-- Main program
main = do
    args <- getArgs
    (param, lines) <- loadData args
 
    (states, alpha, firstState, finalStates, rules) <- checkSyntax lines
    let automatDKA = (makeAutomata states alpha firstState finalStates (makeRules rules))
   
    if param == "-i" then -- ak je zadaný parameter -i -> vypíše sa automata
        printAutomata automatDKA
    else -- ak je zadaný parameter -t -> autoamt sa minimalizuje a vypíše
        printAutomata (minimalisationAutomata (deleteNoReachableState automatDKA) )