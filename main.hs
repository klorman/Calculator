type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
        ("+", (+)),
        ("-", (-)),
        ("*", (*)),
        ("/", (/))
    ] 

numbers :: String -> [String]
numbers [] = [""]
numbers (x:xs) 
        | x == '+' = "" : "+" : rest
        | x == '-' = "" : "-" : rest
        | x == '*' = "" : "*" : rest
        | x == '/' = "" : "/" : rest
        | otherwise = (x : head rest) : tail rest
    where
        rest = numbers xs

calculate :: String -> String
calculate = show . evaluate operatorRegister . numbers

evaluate :: Register -> [String] -> Double
evaluate _ [number] = read number
evaluate ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> evaluate rest unparsed
        (beforeOperator, afterOperator) -> 
            function
                (evaluate operatorRegister beforeOperator)
                (evaluate operatorRegister $ drop 1 afterOperator)

main :: IO()
main = interact (unlines . (map calculate) . lines)