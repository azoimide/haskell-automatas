module Auto.DFA.TestDFAs (
    ex2_1,
    ex2_1demo
) where 

import Auto.DFA.DFA

ex2_1 :: DFA
ex2_1 = setInitial q0 $ markAccepting q3 $ markAccepting q4 $ foldl (.) id (map addState qs) newDFA
    where
    q0 = addTrans "1" "q2" $ addTrans "0" "q1" $ newState "q0"
    q1 = addTrans "0" "q2" $ addTrans "1" "q3" $ newState "q1"
    q2 = addTrans "1" "q4" $ addTrans "0" "q2" $ newState "q2"
    q3 = addTrans "01" "q3" $ newState "q3"
    q4 = addTrans "01" "q4" $ newState "q4"
    qs = [q0, q1, q2, q3, q4]

ex2_1demo :: IO ()
ex2_1demo = do
    putStrLn $ show ex2_1
    let s1 = "100000"
    putStrLn s1
    putStrLn $ show $ accepts ex2_1 s1
    let s2 = "1000001"
    putStrLn s2
    putStrLn $ show $ accepts ex2_1 s2


