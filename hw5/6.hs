data Expr a = Const a |
        Sum (Expr a) (Expr a) |
        Subtr (Expr a) (Expr a) |
        Mult (Expr a) (Expr a) |
        Div (Expr a) (Expr a) |
        Degree (Expr a) Int |
        Variable 
                    
simplified (Sum a (Const 0)) = a
simplified (Sum (Const 0) a) = a
simplified (Subtr a (Const 0)) = a
simplified (Subtr (Const 0) a) = Mult(Const (-1)) a
simplified (Mult a (Const 1)) = a
simplified (Mult (Const 1) a) = a
simplified (Mult a (Const 0)) = Const 0
simplified (Mult (Const 0) a) = Const 0
simplified (Mult (Const a) (Const b)) = Const (a*b)
simplified (Div a (Const 1)) = a
simplified (Div (Const 0) a) = Const 0
simplified (Div a (Const 0)) = error "Division by zero!"
simplified (Degree (Const 0) a) = Const 0 
simplified (Degree a 0) = Const 1
simplified (Degree a 1) = a
simplified (Degree (Const 1) a) = Const 1
simplified a = a

simpleExpr (Sum x y) = simplified (Sum (simpleExpr x) (simpleExpr y))
simpleExpr (Subtr x y) = simplified (Subtr (simpleExpr x) (simpleExpr y))
simpleExpr (Mult x y) = simplified (Mult (simpleExpr x) (simpleExpr y))
simpleExpr (Div x y) = simplified (Div (simpleExpr x) (simpleExpr y))
simpleExpr (Degree x n) = simplified (Degree (simpleExpr x) n)
simpleExpr a = a


derive (Const _) = Const 0
derive Variable = Const 1
derive (Sum x y) = Sum (derive x) (derive y)
derive (Subtr x y) = Subtr (derive x) (derive y)
derive (Mult x y) = Sum (Mult x (derive y)) (Mult y (derive x))
derive (Degree Variable 1) = Const 1
derive (Degree Variable n) = Mult (Const n) (Degree Variable (n - 1))
derive (Degree x n) = Mult (Const n) (Mult (Degree x (n - 1)) (derive x))
derive (Div x y) = Div (Subtr (Mult (derive x) y) (Mult x (derive y))) (Degree y 2)

getInBrac expr = "(" ++ show expr ++ ")"

instance Show a => Show (Expr a) where
            show (Const a) = show a
            show Variable = "x"
            show (Sum x y) = show x ++ " + " ++ show y
            show (Mult x y) = showMult x ++ " * " ++ showMult y
                where showMult (Sum x y) = getInBrac (Sum x y)
                      showMult (Subtr x y) = getInBrac (Subtr x y)
                      showMult (Div x y) = getInBrac (Div x y)
                      showMult x = show x
            show (Subtr x y) = show x ++ " - " ++ showSubtr y
                where showSubtr (Sum x y) = getInBrac (Sum x y)
                      showSubtr (Subtr x y) = getInBrac (Subtr x y)
                      showSubtr x = show x
            show (Div x y) = showDiv x ++ " / " ++ showDiv y
                where showDiv (Sum x y) = getInBrac (Sum x y)
                      showDiv (Subtr x y) = getInBrac (Subtr x y)
                      showDiv (Mult x y) = getInBrac (Mult x y)
                      showDiv (Div x y) = getInBrac (Div x y)
                      showDiv x = show x
            show (Degree x y) = showDegree x ++ " ^ " ++ show y
                where showDegree (Sum x y) = getInBrac (Sum x y)
                      showDegree (Subtr x y) = getInBrac (Subtr x y)
                      showDegree (Mult x y) = getInBrac (Mult x y)
                      showDegree (Div x y) = getInBrac (Div x y)
                      showDegree (Degree x y) = getInBrac (Degree x y)
                      showDegree x = show x
main = do
    let expr1 = Mult (Const 5) Variable
    putStr ("(5 * x)' = ")
    putStrLn (show $ simpleExpr (derive expr1))
    let expr2 = Degree (Sum (Degree Variable 2) (Sum (Mult (Const 2) Variable) (Const 1))) 2
    putStr ("((x ^ 2 + 2 * x + 1) ^ 2)' = ")
    putStrLn (show $ simpleExpr (derive expr2))