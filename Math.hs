import Data.Maybe
import Data.List.Split

data Expr = Num Double 
	| Pow String Integer 
	| Parenth Expr
	| Mul [Expr]
	| Add [Expr]
	| Div Expr Expr 
	| Der Expr 
	| Sin Expr 
	| Cos Expr deriving (Eq,Show)

isNum :: Expr -> Bool
isNum (Num _) = True
isNum _     = False

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

-----------------------------------------------------------------------
--Part for handling users input (making users input into my data format)
-----------------------------------------------------------------------
-- data Stack a = Stack [a] deriving Show

-- emptyStack :: Stack a
-- emptyStack = Stack [] 

-- push :: a -> Stack a -> Stack a
-- push val (Stack s) = Stack(val:s)

-- pop :: Stack a -> (Maybe a, Stack a)
-- pop (Stack []) = (Nothing, Stack [])
-- pop (Stack (x:xs)) = (Just x, Stack xs)

-- data Token = TokenVarX | TokenNum Double | TokenPlus | TokenMinus | TokenMultiply | TokenDivide | TokenEqual | TokenSin | TokenCos | TokenDer | TokenOB | TokenCB | TokenPow | TokenNone | TokenExpr Expr deriving (Show,Eq)

-- tokenize :: [Char] -> Token
-- tokenize "x" = TokenVarX
-- tokenize "sin" = TokenSin
-- tokenize "cos" = TokenCos
-- tokenize "(" = TokenOB
-- tokenize ")" = TokenCB
-- tokenize "+" = TokenPlus
-- tokenize "-" = TokenMinus
-- tokenize "*" = TokenMultiply
-- tokenize "/" = TokenDivide
-- tokenize "'" = TokenDer
-- tokenize "^" = TokenPow
-- tokenize "" = TokenNone
-- tokenize a = (TokenNum (read a :: Double))

-- removeEmptyStrings :: [Char] -> [Char]
-- removeEmptyStrings [] lsNew = reverse $ lsNew
-- removeEmptyStrings ls lsNew = if h/="" then removeEmptyStrings (tail ls) (h:lsNew) else removeEmptyStrings (tail ls) lsNew
-- 	where h = (head ls)
--END
-----------------------------------------------------------------------
--Core of the program, reduction rules for expressions
-----------------------------------------------------------------------
reduce :: Expr -> Expr
reduce (Pow x n) 
	| n == 0 = (Num 1)
	| otherwise = (Pow x n)
reduce (Num n) = (Num n)
--Unnecessary parantheses (not finished)
reduce (Parenth (Mul ls)) = (Mul ls)
reduce (Parenth (Num n)) = (Num n)
-- reduce (Parenth (Num n))
-- 	| n>=0 = (Num n)
-- 	| otherwise = (Parenth (Num n))
reduce (Parenth (Pow x n)) = reduce (Pow x n)

{- Additions: (not finished)
	- Numbers -> addAllNums
	- Xs -> addAllXs (Current: adds only "x"; TODO: should be able to add x^n, E*x, E*x^n)
	- Trigonometry -> TODO
	- Derivatives -> probably wont be implemented
-}
reduce (Add exps)
	| length exps == 1 = reduce (head exps)
	| containsAddExpr exps = reduce (Add (extractExpsFromAdds exps))
	| isXsExists == False && isNumsExits == False = (Add (map reduce exps))
	| isXsExists == False && isNumsExits == True =  (Add ((Num (addAllNums exps)):(map reduce (withoutNums exps))))
	| isXsExists == True && isNumsExits == False = (Add (((extractionOfX exps)):(map reduce (withoutXs exps))))
	| isXsExists == True && isNumsExits == True = (Add ((Num (addAllNums exps)):(((extractionOfX exps)):(map reduce (withoutNums $ withoutXs exps)))))
		where 
			isXsExists = howManyHasX exps > 1
			isNumsExits = containsNums exps

{- Multiplications: (not finished)
	- Numbers -> mulAllNums
	- Xs -> mulAllXs (TODO: pay attention on Mul [x,1,(Mul [2,x]),1,(Mul [5,x^4]),(Mul [6,x,E])])
	- Trigonometry -> 
-}
reduce (Mul exps) 
	| length exps == 1 = reduce (head exps)
	| containsNul exps = (Num 0) 
	| containsMulExpr exps = reduce (Mul (extractExpsFromMuls exps))
	| containsOnes exps = (Mul (map reduce (removeOnes exps)))
	| isXsExists == False && isNumsExits == False = Mul (map reduce exps)
	| isXsExists == False && isNumsExits == True = Mul ((mulAllNums exps):(map reduce (withoutNums exps)))
	| isXsExists == True && isNumsExits == False = Mul ((mulAllXs exps):(map reduce (withoutXsMul exps)))
	| isXsExists == True && isNumsExits == True = Mul ((mulAllNums exps):((mulAllXs exps):(map reduce (withoutNums (withoutXsMul exps)))))
		where 
			isXsExists = xsForMultiplying exps > 1
			isNumsExits = containsNums exps

reduce (Parenth e) = (Parenth (reduce e))
--derivatives TODO:
reduce (Der (Num n)) = (Num 0)
reduce (Der (Pow x n)) 
	| n == 1 = (Num 1)
	| otherwise = (Mul [(Num (fromIntegral n)), (Pow x (n-1))])
reduce (Der (Parenth e)) = reduce (Der e)
reduce (Der (Sin exps)) = (Mul [(Cos exps), (Der exps)])
reduce (Der (Cos exps)) = (Mul [(Num (-1)), (Sin exps), (Der exps)])
reduce (Der (Add exps)) = (Add (map reduce (derivateSummants exps)))
-- reduce (Der (Mul exps)) = ()
reduce e = reduce e

derivateSummants :: [Expr] -> [Expr]
derivateSummants [] = []
derivateSummants (h:t) = (Der h):(derivateSummants t)

withoutXsMul :: [Expr] -> [Expr]
withoutXsMul [] = []
withoutXsMul ((Pow x n):t) = withoutXsMul t
withoutXsMul (h:t) = h:(withoutXsMul t)

mulAllXs :: [Expr] -> Expr
mulAllXs ls = mulAllXs' ls 0

mulAllXs' :: [Expr] -> Integer -> Expr
mulAllXs' [] sum = (Pow "x" sum)
mulAllXs' ((Pow x n):t) sum = mulAllXs' t (sum+n)
mulAllXs' (_:t) sum = mulAllXs' t sum


mulAllNums :: [Expr] -> Expr
mulAllNums ls = mulAllNums' ls 1

mulAllNums' :: [Expr] -> Double -> Expr
mulAllNums' [] res = Num res
mulAllNums' ((Num n):t) res = mulAllNums' t (res*n)
mulAllNums' (_:t) res = mulAllNums' t res


xsForMultiplying :: [Expr] -> Integer
xsForMultiplying [] = 0
xsForMultiplying ((Pow x n):t) = 1 + xsForMultiplying t
xsForMultiplying (_:t) = xsForMultiplying t

extractExpsFromMuls :: [Expr] -> [Expr]
extractExpsFromMuls ls = extractExpsFromMuls' ls [] 

extractExpsFromMuls' :: [Expr] -> [Expr] -> [Expr]
extractExpsFromMuls' [] newLs = newLs
extractExpsFromMuls' ((Mul ls):t) newLs = extractExpsFromMuls' t (merge ls newLs)
extractExpsFromMuls' (h:t) newLs = extractExpsFromMuls' t (h:newLs)

extractExpsFromAdds :: [Expr] -> [Expr]
extractExpsFromAdds ls = extractExpsFromAdds' ls [] 

extractExpsFromAdds' :: [Expr] -> [Expr] -> [Expr]
extractExpsFromAdds' [] newLs = newLs
extractExpsFromAdds' ((Add ls):t) newLs = extractExpsFromAdds' t (merge ls newLs)
extractExpsFromAdds' (_:t) newLs = extractExpsFromAdds' t newLs

removeOnes :: [Expr] -> [Expr]
removeOnes [] = []
removeOnes ((Num 1):t) = removeOnes t
removeOnes (e:t) = e:(removeOnes t)

containsMulExpr :: [Expr] -> Bool
containsMulExpr [] = False
containsMulExpr ((Mul ls):t) = True
containsMulExpr (_:t) = containsMulExpr t

containsAddExpr :: [Expr] -> Bool
containsAddExpr [] = False
containsAddExpr ((Add ls):t) = True
containsAddExpr (_:t) = containsAddExpr t

containsOnes :: [Expr] -> Bool
containsOnes [] = False
containsOnes ((Num 1):t) = True
containsOnes (e:t) = containsOnes t

containsNul :: [Expr] -> Bool
containsNul [] = False
containsNul ((Num 0):t) = True
containsNul (e:t) = containsNul t

containsNums :: [Expr] -> Bool
containsNums [] = False
containsNums ((Num n):t) = True
containsNums (e:t) = containsNums t

howManyHasX :: [Expr] -> Integer
howManyHasX [] = 0
howManyHasX exps@((Pow x n):_) = if n>0 then 1+howManyHasX (tail exps) else howManyHasX (tail exps)
howManyHasX exps@((Mul ls):_) = (howManyHasX ls) + howManyHasX (tail exps)
howManyHasX exps@(_:_) = 0+howManyHasX (tail exps)

withoutNums :: [Expr] -> [Expr]
withoutNums ls = withoutNums' ls []

withoutNums' :: [Expr] -> [Expr] -> [Expr]
withoutNums' [] without = without
withoutNums' exps@((Num n):_) without = withoutNums' (tail exps) without
withoutNums' exps@(e:_) without = withoutNums' (tail exps) (e:without)

withoutXs :: [Expr] -> [Expr]
withoutXs ls = withoutXs' ls []

withoutXs' :: [Expr] -> [Expr] -> [Expr]
withoutXs' [] without = without
withoutXs' exps@((Pow x n):_) without = withoutXs' (tail exps) without
withoutXs' exps@((Mul ls):_) without = if (howManyHasX ls) > 0 then withoutXs' (tail exps) without else withoutXs' (tail exps) ((Mul ls):without)
withoutXs' exps@(e:_) without = withoutXs' (tail exps) (e:without)

addAllNums :: [Expr] -> Double 
addAllNums [] = 0
addAllNums ls@((Num n):_) = n + (addAllNums $ tail ls)
addAllNums ls@(_:_) = 0 + (addAllNums $ tail ls)

extractionOfX :: [Expr] -> Expr
extractionOfX ls = extractionOfX' ls (Mul ((Parenth (Add [])):((Pow "x" 1)):[]))

extractionOfX' :: [Expr] -> Expr -> Expr
extractionOfX' [] e = e
extractionOfX' ((Pow "x" n):t) (Mul ((Parenth (Add res)):((Pow "x" 1):[]))) = if n==1 then extractionOfX' t (Mul ((Parenth (Add ((Num 1):res))):((Pow "x" 1):[]))) else extractionOfX' t (Mul ((Parenth (Add ((Pow "x" (n-1)):res))):((Pow "x" 1):[])))
extractionOfX' ((Mul ls):t) (Mul ((Parenth (Add res)):((Pow "x" 1):[]))) = if containsX ls then extractionOfX' t (Mul ((Parenth (Add ((extractOneX ls):res))):((Pow "x" 1):[]))) else extractionOfX' t (Mul ((Parenth (Add (res))):((Pow "x" 1):[])))
extractionOfX' (_:t) e = extractionOfX' t e

containsX :: [Expr] -> Bool
containsX [] = False
containsX ((Pow x n):_)
	| n<1 = False
	| otherwise = True
containsX ((Mul ls):t) = containsX ls || containsX t
containsX (h:t) = containsX t

extractOneX :: [Expr] -> Expr
extractOneX ls = extractOneX' ls []				

extractOneX' :: [Expr] -> [Expr] -> Expr
extractOneX' ((Pow x n):t) collector
	| n < 1 = extractOneX' t ((Pow x n):collector)
	| n == 1 = if isSingleX 
		then (Num 1) 
		else if isXAndOneSmth
			then if length t == 1 then head t else head collector	
			else (Mul (merge t collector)) --isXAndManySmth
	| otherwise = if isSingleX
		then (Pow x (n-1)) 
		else (Mul ((Pow x (n-1)):(merge t collector)))
		where 
			isSingleX = (length t + length collector) == 0
			isXAndOneSmth = (length t + length collector) == 1
			isXAndLotOfSmth = (length t + length collector) > 1
extractOneX' (h:t) collector = (extractOneX' t (h:collector))



reduceNow :: Expr -> Expr
reduceNow e = reduceNow' e (reduce e)
reduceNow' previous current = if previous == current then current else reduceNow' current next
	where next = reduce current

reduceLoop :: Expr -> [Expr] 
reduceLoop e = reverse $ reduceLoop' e [e]
	where reduceLoop' e ls@(h:_) = if r == h then ls else reduceLoop' r (r:ls)
		where r = reduce e
--END

-----------------------------------------------------------------------
--Part for showing the result of the program
-----------------------------------------------------------------------
printExpr :: Expr -> String
printExpr (Parenth e) = "("++printExpr e++")"
printExpr (Pow x n) 
	| n < 0 = "x^{"++show n++"}"
	| n == 1 = "x"
	| otherwise = "x^"++show n
printExpr (Num n)
	| ((fromIntegral $ floor n) :: Double) - n == 0 = if n<0 then "("++ (show $ floor n) ++")" else (show $ floor n)
	| otherwise = if n<0 then "("++ show n ++ ")" else show n
printExpr (Add ls) = (printList ls "+")
printExpr (Mul ls) = (printList ls "*")
printExpr (Der e) = printExpr e++"'"
printExpr _ = "err"

printList :: [Expr] -> String -> String
printList ls@(h:_) symbol
	| ((length ls) == 0) = ""
	| ((length ls) <= 1) = (printExpr h)
	| otherwise = (printExpr h)++symbol++(printList (tail ls) symbol)



printExprLoop :: [Expr] -> String
printExprLoop ls = printExprLoop' ls " "
printExprLoop' [] _ = ""
printExprLoop' (h:t) previous = if (printExpr h) == previous then printExprLoop' t (printExpr h) else "\\begin{equation}\n=" ++ printExpr h ++ "\n\\end{equation}\n" ++ printExprLoop' t (printExpr h)

--END
-----------------------------------------------------------------------
--Examples for testing the program
-----------------------------------------------------------------------
one = (Num 1.0)
two = (Num 2.0)
three = (Num 3.0)
four = (Num 4.0)
minusOne = (Num (-1.0))
minusTwo = (Num (-2.0))
minusThree = (Num (-3.0))
minusFour = (Num (-4.0))
x = (Pow "x" 1)
example0 = x
--Number additions
--1+2
example1 = (Add [minusOne,two,four])
--1+(2+3)
example2 = (Add [one, (Parenth (Add [two, three]))])
-- --(1+2)+(3+4)
-- example3 = (Add (AddParenth (Num 1.0) (Num 2.0)) (AddParenth (Num 3.0) (Num 4.0)))
--(1+2+3) 
example4 = (Parenth (Add [one,three,minusTwo]))
--e2,(e4,e1)
example5 = (Add [example2, (Parenth (Add [example4, example1]))])
--Number subtractions
--1-2
example6 = (Add [(Num 1.0),(Num (-2.0))])
-- -1+2
example7 = (Add [(Num (-1.0)),(Num 2.0)])
-- -1-2-3-4
example8 = (Add [minusOne, minusTwo, minusThree, minusFour])
-- (1-4)-4
example9 = (Add [(Parenth (Add [one,minusFour])), minusFour])
-- (1-2)-(3-4)?? - nakon sto dodam mnozenje
example10 = (Add [(Parenth (Add [one, minusTwo])) , (Mul [minusOne,(Parenth (Add [three, minusFour]))])])
-- --Number multiplications
-- --1*2*3*4
example11 = (Mul [one,two,three,four])
-- --2*(1-3)*(1-2+4)
example12 = (Mul [two, (Parenth (Add [one, minusThree])), (Parenth (Add [one, minusTwo, four]))])
-- --2+3*(1-2)+(2-2)+(-3-1)*1*2*3
example13 = (Add [two, 
				(Mul [three, (Parenth (Add [one, minusTwo]))]),
				(Parenth (Add [two, minusTwo])),
				(Mul [(Parenth (Add [minusThree, minusOne])), 
					one, 
					two,
					three
					])
				])
-- Variable additions
--x+x
example14 = (Add [x,x])
----1+x+2+x
example15 = (Add [one,x,two,x])
----2x+x-4x+x
example16 = (Add [(Mul [x,two]),x,(Mul [minusFour,x]),x])
--x+4x+4-2+2x
example17 = (Add [x,(Mul [four,x]),minusTwo,(Mul [minusTwo,x])])
-- e14+e15+e16+e17
example18 = (Add [example14,example15,example16,example17])
-- x+x*x
example19 = (Add [x,(Mul [x,x])])
-- x*x+x*x*x+x
example20 = (Add [(Mul [x,x]),(Mul [x,x,x]),x])
-- x^2+x
example21 = (Add [(Pow "x" 2),x])
-- x^3+x^2
example22 = (Add [(Pow "x" 2),(Mul [two,(Pow "x" 2)])])
-- e21+e22
example23 = (Add [example21,example22,example20])
-- x*x+x*x*x+x^2
example24 = (Add [(Mul [x,x]),(Mul [x,x,x]),(Pow "x" 2)])
-- (-3)'
example25 = (Der minusThree)
-- x'
example26 = (Der x)
-- (x^-3)'
example27 = (Der (Parenth (Pow "x" (-3))))
-- (x^2+1+x+x^2)'
example28 = (Der (Parenth (Add [(Pow "x" (2)), one, x, (Pow "x" 2)])))
-- 


----END
main = do
	-- let a = reduceLoop example23
	-- print $ a
	-- putStrLn "Input math expression you want to be reduced!"
	-- usersInput <- getLine
	-- let parts = (split (oneOf "+-/*^()") usersInput)
	-- print parts
	-- --get rid of empty strings
	-- let cleanParts = removeEmptyStrings parts []
	-- print cleanParts
	-- let tokens = map tokenize cleanParts
	-- print tokens
	-- let parsedTokens = parse tokens
	-- print parsedTokens

	
	
	writeFile "outputTest.tex" 
		("\\documentclass{article}\\usepackage{amsmath}\\begin{document}\n\\subparagraph{Example 0}\n\\begin{equation}\n" 
		++ (printExpr $ head (reduceLoop example0)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example0)) 
		++ "\\subparagraph{Example 1}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example1)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example1))
		++ "\\subparagraph{Example 2}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example2)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example2))
		++ "\\subparagraph{Example 4}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example4)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example4))
		++ "\\subparagraph{Example 5}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example5)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example5))
		++ "\\subparagraph{Example 6}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example6)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example6))
		++ "\\subparagraph{Example 7}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example7)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example7))
		++ "\\subparagraph{Example 8}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example8)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example8))
		++ "\\subparagraph{Example 9}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example9)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example9))
		++ "\\subparagraph{Example 10}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example10)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example10))
		++ "\\subparagraph{Example 11}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example11)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example11))
		++ "\\subparagraph{Example 12}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example12)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example12))
		++ "\\subparagraph{Example 13}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example13)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example13))
		++ "\\subparagraph{Example 14}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example14)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example14))
		++ "\\subparagraph{Example 15}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example15)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example15))
		++ "\\subparagraph{Example 16}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example16)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example16))
		++ "\\subparagraph{Example 17}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example17)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example17))
		++ "\\subparagraph{Example 18}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example18)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example18))
		++ "\\subparagraph{Example 19}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example19)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example19))
		++ "\\subparagraph{Example 20}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example20)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example20))
		++ "\\subparagraph{Example 21}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example21)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example21))
		++ "\\subparagraph{Example 22}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example22)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example22))
		++ "\\subparagraph{Example 23}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example23)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example23))
		++ "\\subparagraph{Example 24}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example24)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example24))
		++ "\\subparagraph{Example 25}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example25)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example25))
		++ "\\subparagraph{Example 26}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example26)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example26))
		++ "\\subparagraph{Example 27}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example27)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example27))
		++ "\\subparagraph{Example 28}\n\\begin{equation}\n"
		++ (printExpr $ head (reduceLoop example28)) ++ "\n\\end{equation}\n"  
		++ (printExprLoop $ tail (reduceLoop example28))
		-- ++ "\\subparagraph{Example 29}\n\\begin{equation}\n"
		-- ++ (printExpr $ head (reduceLoop example29)) ++ "\n\\end{equation}\n"  
		-- ++ (printExprLoop $ tail (reduceLoop example29))
		-- ++ "\\subparagraph{Example 30}\n\\begin{equation}\n"
		-- ++ (printExpr $ head (reduceLoop example30)) ++ "\n\\end{equation}\n"  
		-- ++ (printExprLoop $ tail (reduceLoop example30))
		
		++ "\\end{document}")

-- Mul [Mul [Parenth (Add [Num 2.0,Num 1.0]),Pow "x" 1],Pow "x" 1],

-- Mul [Parenth (Add [Num 3.0]),Pow "x" 1],

-- Mul [Parenth (Num 3.0),Pow "x" 1],

-- Mul [Num 3.0,Pow "x" 1]]
