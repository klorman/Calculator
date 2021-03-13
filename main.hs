type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
        ("(", undefined),
        (")", undefined),
        ("[", undefined),
        ("]", undefined),
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
        | x == ' ' = numbers xs prev
        | x == '+' = if prev then "" : "+" : rest else "+" : rest
        | x == '-' = if prev then "" : "-" : rest else "-" : rest
        | x == '*' = if prev then "" : "*" : rest else "*" : rest
        | x == '/' = if prev then "" : "/" : rest else "/" : rest
        | x == '^' = if prev then "" : "^" : rest else "^" : rest
        | x == '(' = if prev then "" : "(" : rest else "(" : rest
        | x == ')' = if prev then "" : ")" : rest else ")" : rest
        | x == '[' = if prev then "" : "[" : rest else "[" : rest
        | x == ']' = if prev then "" : "]" : rest else "]" : rest
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
parenthesis ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> parenthesis rest unparsed
        (left, right)
            | operator == "(" -> parenthesis operatorRegister (left ++ parenthesis operatorRegister (tail right))
            | operator == ")" -> ([show (evaluate operatorRegister left)]) ++ (tail right)
            | otherwise -> error "ERROR: Invalid parenthesis placement!"

calculate :: String -> String
calculate str = show ( evaluate operatorRegister $ numbers str False)

evaluate :: Register -> [String] -> Double
evaluate _ [left, right]
    | left == "+" =   evaluate operatorRegister [right]
    | left == "-" = - evaluate operatorRegister [right]
    | left == "(" || left == ")" || right == "(" || right == ")" = error "ERROR: Invalid parenthesis placement!"
    | left == "*" || left == "/" || left == "^" = error "ERROR: Invalid use of the operator!"
    | otherwise = error "ERROR: Invalid value!"

evaluate _ [number]
    | isNumber number = read number
    | otherwise = error "ERROR: Invalid value!"

evaluate ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> evaluate rest unparsed
        (left, right)
            |  operator == "(" -> evaluate operatorRegister (left ++ (parenthesis operatorRegister $ tail right))
            | otherwise -> function 
            (evaluate operatorRegister left) 
            (evaluate operatorRegister $ tail right)

main :: IO()
main = interact (unlines . (map calculate) . lines)
