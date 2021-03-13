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

numbers :: String -> [String]
numbers [] = [""]
numbers (x:xs) 
        | x == ' ' = rest
        | x == '+' = "" : "+" : rest
        | x == '-' = "" : "-" : rest
        | x == '*' = "" : "*" : rest
        | x == '/' = "" : "/" : rest
        | x == '^' = "" : "^" : rest
        | x == '(' =      "(" : rest
        | x == ')' = "" : ")" : rest
        | x == '[' = "" : "[" : rest
        | x == ']' = "" : "]" : rest
        | otherwise = (x : head rest) : tail rest
    where
        rest = numbers xs

parenthesis :: Register -> [String] -> [String]
parenthesis _ [""] = error "ERROR: incorrect placement of parentheses"
parenthesis ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> parenthesis rest unparsed
      --  ([""], _) -> []   
        (left, right)
            | operator == "(" -> parenthesis operatorRegister (left ++ parenthesis operatorRegister (tail right))
            | operator == ")" -> ([show (evaluate operatorRegister left)]) ++ (drop 2 right)
            | otherwise -> error "ERROR: incorrect placement of parentheses"

calculate :: String -> String
calculate = show . evaluate operatorRegister . numbers

evaluate :: Register -> [String] -> Double
evaluate _ [number] = read number
evaluate ((operator, function):rest) unparsed =
    case span (/= operator) unparsed of
        (_, []) -> evaluate rest unparsed
        (left, right)
            | operator == "(" -> evaluate operatorRegister (left ++ (parenthesis operatorRegister $ tail right))
            | otherwise -> function (evaluate operatorRegister left) (evaluate operatorRegister $ tail right)

main :: IO()
main = interact (unlines . (map calculate) . lines)
