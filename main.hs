type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
        (")", undefined),
        ("(", undefined),
        ("+", (+)),
        ("-", (-)),
        ("*", (*)),
        ("/", (/)),
        ("^", (**))
    ] 

numbers :: String -> Bool -> [String]
numbers [] True  = [""]
numbers [] False = []
numbers (x:xs) prev
        | x == ' ' = if prev then "" :       rest else       rest
        | x == '+' = if prev then "" : "+" : rest else "+" : rest
        | x == '-' = if prev then "" : "-" : rest else "-" : rest
        | x == '*' = if prev then "" : "*" : rest else "*" : rest
        | x == '/' = if prev then "" : "/" : rest else "/" : rest
        | x == '^' = if prev then "" : "^" : rest else "^" : rest
        | x == '(' = if prev then "" : "(" : rest else "(" : rest
        | x == ')' = if prev then "" : ")" : rest else ")" : rest
        | otherwise = (x : head nrest) : tail nrest
    where
        rest  = numbers xs False
        nrest = numbers xs True

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False

addToTuple :: ([String], [String]) -> String -> Bool -> ([String], [String])
addToTuple (left, right) elem pos
    | pos       = (left, right ++ [elem])
    | otherwise = (elem : left, right)

mySpan :: (String -> Bool) -> [String] -> ([String], [String])
mySpan _ [] = ([], [])
mySpan predic lst = if predic $ last lst
    then addToTuple (mySpan predic $ init lst) (last lst) True
    else (lst, [])

parenthesis :: Register -> [String] -> [String]
parenthesis _ [""] = error "ERROR: Invalid parenthesis placement!"
parenthesis [] _   = error "ERROR: Invalid parenthesis placement!"
parenthesis ((operator, function):rest) unparsed =
    case mySpan (/= operator) unparsed of
        ([], _) -> parenthesis rest unparsed
        (left, right)
            | operator == ")" -> parenthesis operatorRegister ((parenthesis operatorRegister $ init left) ++ right)
            | operator == "(" -> (init left) ++ [show (evaluate operatorRegister right)]
            | otherwise -> error "ERROR: Invalid parenthesis placement!"

calculate :: String -> Double
calculate str = evaluate operatorRegister $ numbers str False

evaluate :: Register -> [String] -> Double
evaluate _ [] = error "ERROR: Invalid syntax!"
evaluate _ [left, right]
    | left == "+" =   evaluate operatorRegister [right]
    | left == "-" = - evaluate operatorRegister [right]
    | left == "(" || left == ")" || right == "(" || right == ")" = error "ERROR: Invalid parenthesis placement!"
    | left == "*" || left == "/" || left  == "^"                 = error "ERROR: Invalid use of the operator!"
    | otherwise = error "ERROR: Invalid value!"

evaluate _ [number]
    | isNumber number = read number
    | otherwise = error "ERROR: Invalid value!"

evaluate ((operator, function):rest) unparsed =
    case mySpan (/= operator) unparsed of
        ([], _) -> evaluate rest unparsed
        (left, right)
            | operator == ")" -> evaluate operatorRegister ((parenthesis (take 2 operatorRegister) $ init left) ++ right)    
            | otherwise -> function (evaluate operatorRegister $ init left) (evaluate operatorRegister right)

main :: IO()
main = interact $ unlines . (map (show . calculate)) . lines
