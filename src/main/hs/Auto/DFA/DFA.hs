module Auto.DFA.DFA (
    DFA,
    State,
    Transition,

    -- fuctions related to Transition
    nextState,
    symbols,

    -- functions related to State
    label,
    trans,

    -- functions related to DFA
    initial,
    accepting,
    states,

    -- functions to manipulate DFAs
    newDFA,
    addState,
    addStates,
    setInitial,
    markAccepting,

    -- functions to do things with states
    newState,
    addTrans,

    -- functions for using DFAs
    step,
    traverse,
    accepts
) where

--import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck hiding (label, labels)
import Control.Monad
import Data.List.Split

type Label = String

data Transition = Transition {
    nextState :: Label,
    symbols :: [Char]
} deriving Show

data State = State {
    label :: Label,
    trans :: [Transition]
} deriving Show

instance Eq State where
    s1 == s2 = (label s1) == (label s2)

instance Arbitrary State where
    arbitrary = fmap newState arbitrary

data DFA = DFA {
    initial :: Maybe State,
    accepting :: [State],
    states :: [State]
}

instance Arbitrary DFA where
    arbitrary = do
        size <- suchThat (arbitrary :: Gen Int) (\x -> x >= 1 && x <= 5)
        let alph = "012"
        let labels = [ "q" ++ (show i) | i <- [1..size] ]
        let transs = [ (lab,sym) | lab <- labels, sym <- alph ]
        from <- shuffle $ map fst transs
        let transs2 = zip from $ chunksOf (length alph) transs

        let ss = map (\(f,ts) -> (foldl (.) id (map (\(t,c) -> addTrans [c] t) ts)) (newState f)) transs2

        ini <- fmap head $ shuffle ss
        noAcc <- suchThat (arbitrary :: Gen Int) (\x -> x >= 1 && x <= 3)
        acc <- fmap (take noAcc) $ shuffle ss

        return $ setInitial ini $ markAcceptings acc $ addStates ss newDFA 


instance Show DFA where
    show a = "DFA {initial = " ++ (show $ fmap label $ initial a) ++ ", accepting = " ++ (show $ map label $ accepting a) ++ ", states = " ++ (show $ states a) ++ "}"

newDFA :: DFA
newDFA = DFA Nothing [] []

addState :: State -> DFA -> DFA
addState s a
    | s `elem` (states a) = error ((show s) ++ " already in " ++ (show a))
    | otherwise = DFA (initial a) (accepting a) (s:(states a))

addStates :: [State] -> DFA -> DFA
addStates ss a = foldl (.) id (map (\s -> addState s) ss) a

findState :: DFA -> Label -> Maybe State
findState a l = foldl mplus Nothing $ map (\s -> if label s == l then Just s else Nothing) (states a)

setInitial :: State -> DFA -> DFA
setInitial s a
    | s `elem` (states a) = DFA (Just s) (accepting a) (states a)
    | otherwise = error ((show s) ++ " not in " ++ (show a))

markAccepting :: State -> DFA -> DFA
markAccepting s a
    | s `elem` (states a) = DFA (initial a) (s:(accepting a)) (states a)
    | otherwise = error ((show s) ++ " not in " ++ (show a))

markAcceptings :: [State] -> DFA -> DFA
markAcceptings ss a = foldl (.) id (map (\s -> markAccepting s) ss) a

newState :: String -> State
newState l = State l []

addTrans :: [Char] -> Label -> State -> State
addTrans cs t f = State (label f) ((Transition t cs):(trans f))

step :: State -> Char -> Maybe Label
step s c = foldl mplus Nothing $ map (\t -> if c `elem` symbols t then (Just $ nextState t) else Nothing) $ trans s

traverse :: DFA -> String -> State -> Maybe State
traverse _ [] s = Just s
traverse a (c:cs) s = (step s c) >>= findState a >>= traverse a cs

accepts :: DFA -> String -> Bool
accepts a cs = case (fmap (\ss -> elem ss (accepting a))) s of
                    Just True -> True
                    _ -> False
    where s = (initial a) >>= traverse a cs


























