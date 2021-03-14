type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
        ("(", undefined),
        (")", undefined),
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
        | x == ' ' = if prev then "" : rest else rest
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

parenthesis :: Register -> [String] -> [String]
parenthesis _ [""] = error "ERROR: Invalid parenthesis placement!"
parenthesis [] _   = error "ERROR: Invalid parenthesis placement!"
parenthesis ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> parenthesis rest unparsed
        (left, right)
            | operator == "(" -> parenthesis operatorRegister (left ++ (parenthesis operatorRegister $ tail right))
            | operator == ")" -> ([show (evaluate operatorRegister False left)]) ++ (tail right)
            | otherwise -> error "ERROR: Invalid parenthesis placement!"

calculate :: String -> String
calculate str = show ( evaluate operatorRegister False $ numbers str False)

evaluate :: Register -> Bool -> [String] -> Double
evaluate _ _ [] = error "ERROR: Invalid syntax!"
evaluate _ _ [left, right]
    | left == "+" =   evaluate operatorRegister False [right]
    | left == "-" = - evaluate operatorRegister False [right]
    | left == "(" || left == ")" || right == "(" || right == ")" = error "ERROR: Invalid parenthesis placement!"
    | left == "*" || left == "/" || left == "^" = error "ERROR: Invalid use of the operator!"
    | otherwise = error "ERROR: Invalid value!"

evaluate _ _ [number]
    | isNumber number = read number
    | otherwise = error "ERROR: Invalid value!"

evaluate ((operator, function):rest) sign unparsed =
    case span (/= operator) unparsed of
        (_, []) -> evaluate rest sign unparsed
        (left, right)
            | operator == "(" -> evaluate operatorRegister False (left ++ (parenthesis (take 2 operatorRegister) $ tail right))
            | operator == "-" -> if sign 
                then (+) (evaluate operatorRegister True left) (evaluate operatorRegister True $ tail right) 
                else (-) (evaluate operatorRegister True left) (evaluate operatorRegister True $ tail right)      
            | otherwise -> function (evaluate operatorRegister False left) (evaluate operatorRegister False $ tail right)

main :: IO()
main = interact (unlines . (map calculate) . lines)
