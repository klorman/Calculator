import Debug.Trace

type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]



operatorRegister :: Register
operatorRegister = [
        (".", undefined),
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


parenthesis :: Register -> [String] -> [String]
parenthesis _ [""] = error "ERROR: incorrect placement of parentheses"
parenthesis ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> parenthesis rest unparsed
        --([""], _) -> []   
        (left, right)
            | operator == "(" -> parenthesis operatorRegister (left ++ parenthesis operatorRegister (tail right))
            | operator == ")" -> ([show (evaluate operatorRegister left)]) ++ (tail right)
            | otherwise -> error "ERROR: incorrect placement of parentheses"

calculate :: String -> String
calculate str = show ( evaluate operatorRegister $ numbers str False)

evaluate :: Register -> [String] -> Double
evaluate _ [left, right]
    | left == "+" =   evaluate operatorRegister [right]
    | left == "-" = - evaluate operatorRegister [right]
    | otherwise = error "ERROR: incorrect use of the operator"

evaluate _ [number] = read number
evaluate ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> trace (show unparsed) evaluate rest unparsed
        (left, right)
            |  operator == "(" -> evaluate operatorRegister (left ++ (parenthesis operatorRegister $ tail right))
            | otherwise -> function 
            (evaluate operatorRegister left) 
            (evaluate operatorRegister $ tail right)

main :: IO()
main = interact (unlines . (map calculate) . lines)
