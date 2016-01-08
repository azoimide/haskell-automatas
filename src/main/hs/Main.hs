module Main where

import Auto.DFA.DFA

q0 = addTrans (addTrans (newState "q0") "0" "q1") "1" "q2"
q1 = addTrans (addTrans (newState "q1") "1" "q3") "0" "q2"
q2 = addTrans (addTrans (newState "q2") "0" "q2") "1" "q4"
q3 = addTrans (newState "q3") "01" "q3"
q4 = addTrans (newState "q4") "01" "q4"

a = setInitial q0 $ markAccepting q3 $ markAccepting q4 $ addState q0 $ addState q1 $ addState q2 $ addState q3 $ addState q4 newDFA

main :: IO ()
main = do
    putStrLn $ show a
    let s1 = "100000"
    putStrLn s1
    putStrLn $ show $ accepts a s1
    let s2 = "1000001"
    putStrLn s2
    putStrLn $ show $ accepts a s2

