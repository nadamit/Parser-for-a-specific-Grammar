module Parser where

import Scanner
import Data.Ratio
import Data.List


import Scanner
import Data.Ratio
import Data.List

data Exp = RExp Rational  | 
           Var String     | 
           Sum Exp Exp    | 
           Diff Exp Exp   | 
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp        |
           Let ExpSeq Exp deriving (Eq, Show)

data ExpSeq = Seq [Binding] | ParseEqnSeqError String deriving (Eq, Show)

data Binding = Bind String Exp | Eqn Exp Exp deriving (Eq, Show)


extractContent (Seq a) = a

stringFromToken :: Token -> String
stringFromToken (Compound (Id s)) = s

integerFromToken :: Token -> Rational
integerFromToken (Compound (Num n)) = toRational n

newtype Parser a = Parser ([Token] -> [(a, [Token])])

unWrap (Parser f) = f

instance Monad Parser where
  return a = Parser(\ts->[(a, ts)])
  p >>= f = Parser(\ts->[(b, ts2) | (a, ts1) <- unWrap p ts, (b, ts2) <- unWrap (f a) ts1])

failParser :: Parser a
failParser = Parser(\ts-> [])

-- item makes sense for "token" Parsers
item :: Parser Token
item = Parser(\ts->case ts of [] -> []; t:ts1 -> [(t,ts1)])

parserFilter :: Parser b -> (b -> Bool) -> Parser b
parserFilter parser p = do {a <- parser; if p a then return a else failParser}

literal :: Token -> Parser Token
literal t = parserFilter item (==t)

variable :: Parser Token
variable =  parserFilter item (\tok->case tok of (Compound (Id _)) -> True; _ -> False)

number :: Parser Token
number =  parserFilter item (\tok->case tok of (Compound (Num _)) -> True; _ -> False)

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\ts-> (unWrap p1 ts) ++ (unWrap p2 ts))


getDefs :: Parser ExpSeq
getDefs = getDefs2 []

getDefs2 :: [Binding] -> Parser ExpSeq
getDefs2 revBinds =
  do binding <- getDef
     ((do tok <- (literal (Simple COMMA))
          getDefs2 (binding:revBinds))
       +++
      (return (Seq (reverse (binding:revBinds)))))

getDef :: Parser Binding
getDef = 
  do vtok <- getMathExp
     tok <- (literal (Simple EQ1))
     exp <- getMathExp
     return (Eqn ( vtok) exp)

getMathExp :: Parser Exp
getMathExp = 
  do 
     term <- getTerm
     getMathExp' term

getMathExp' :: Exp -> Parser Exp
getMathExp' term =
  (do tok <- (literal (Simple PLUS)) 
      term2 <- getTerm
      getMathExp' (Sum term term2))
  +++
  (do tok <- (literal (Simple MINUS))
      term2 <- getTerm
      getMathExp' (Diff term term2))
  +++
   (return term)

getTerm :: Parser Exp
getTerm = 
  do factor <- getFactor
     getTerm' factor

getTerm' :: Exp -> Parser Exp
getTerm' factor = 
  (do tok <- (literal (Simple STAR))
      factor2 <- getFactor
      getTerm' (Prod factor factor2))
  +++
  (do tok <- (literal (Simple SLASH))
      factor2 <- getFactor
      getTerm' (Quo factor factor2))
  +++
   (return factor)

getFactor :: Parser Exp
getFactor = 
  (do vtok <- variable
      return (Var (stringFromToken vtok)))
  +++
  (do ntok <- number
      return (RExp (integerFromToken ntok)))
  +++
  (do tok <- (literal (Simple MINUS))
      factor <- getFactor
      return (Neg factor))
  +++
  (do tok <- (literal (Simple OP)) 
      exp <- getMathExp
      tok <- (literal (Simple CP))
      return exp)

parse :: [Token] -> ExpSeq
parse ts =
  case unWrap getDefs ts of
    []            -> ParseEqnSeqError "Bad input" 
    (exp, ts1):ps -> if isEmptyTokenStream ts1
                     then exp
                     else ParseEqnSeqError "Unconsumed input" 

{--
Seq [Eqn (Var "w") (Sum (Prod (Quo (RExp (1 % 1)) (RExp (2 % 1))) (Var "w")) (RExp (20 % 1)))]
--}

parseString :: String -> ExpSeq
parseString = parse . tokenStreamFromString

--a
sub::(Num a)=>[a]->[a]->[a]
sub (x:xs) [] = x:sub xs []
sub [] (y:ys) = y:sub [] ys
sub [] [] = []
sub (x:xs) (y:ys) = (x-y):sub xs ys
{--
sub [3,2,1,9][2,3,4,1]
[1,-1,-3,8]
--}

--b
scaleList::(Num a)=>a->[a]->[a]
scaleList c lst = [c*x|x<-lst]
{--
 scaleList 2 [1,2,3,4]
[2,4,6,8]
--}

--c
subScale::(Fractional a)=>[a]->[a]->[a]
subScale lst1@(x:xs) lst2@(y:ys) = let scalFirstList (m:ms)= (scaleList (y/x) lst1) 
                  in tail (sub lst2 (scalFirstList lst1))
{--
 subScale [1,2,3,4][1,5,6,7]
[3.0,3.0,3.0]  
--}

--d
nonZeroFirst::(Num a,Eq a)=>[[a]]->[[a]]
nonZeroFirst ([[]]) = error "Empty"
nonZeroFirst list   = let check lst = if all(\x-> (head x)==0) lst then error "All lists start with zeros" else lst
                        in (check [x|x<-list, head x /=0]++[y|y<-list, head y == 0])
{--
   nonZeroFirst [[0,0,1,2],[1,2,3,4],[0,3,4,5]]
[[1,2,3,4],[0,0,1,2],[0,3,4,5]]
  --}

--e

triangulate :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangulate list
      |length (last list) == 2 = list
      |otherwise = [head list]++(triangulate(removeZeros(nonZeroFirst(scalify (head (nonZeroFirst list)) (tail (nonZeroFirst list))))))
{--
triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
[[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
--}

scalify::(Fractional a, Eq a)=>[a]->[[a]]->[[a]]
scalify list1 [] = []   
scalify list1 (x:xs) =  [(subScale list1 x)]++(scalify list1 xs)  

{--
scalify [1,2,3,4] [[1,2,3]]
[[0.0,0.0,4.0]]
--}


removeZeros::(Num a,Eq a)=>[[a]]->[[a]]
removeZeros [] = []
removeZeros (x:xs) = [(remZero x)]++removeZeros xs


remZero::(Num a, Eq a)=>[a]->[a]
remZero (x:xs) 
    | x/=0 = x:xs
    |otherwise = remZero xs 
{--remZero [0,2,3,4,0,5,6]
[2,3,4,0,5,6]--}

scale :: Fractional a => [[a]] -> [[a]]
scale (x:y:z:cs) = [x]++[(subScale x y)]++[(subScale x z)]


--f
dot::(Num a)=>[a]->[a]->a
dot [] [] = 0
dot [] _  = 0
dot _  [] = 0
dot (x:xs) (y:ys)= (x*y) + dot xs ys
{--
dot [1,2] [3,4]
11
--}

--g   
solveLine::(Fractional a)=>[a]->[a]->a
solveLine list1 list2= ((last list1)-(dot (calc (tail list1) (length (tail list1)) []) list2))/(head list1) 
{--
solveLine [2,3,3,8] [1,1]
1.0
--}

calc::(Num a, Eq a) => [a1] -> a -> [a1] -> [a1]
calc (x:xs) counter buffer
  |counter==1 = reverse buffer
  |otherwise = calc xs (counter-1) (x:buffer)
  {--
   calc [1,2,3,4] 2 [4,5,6,7]
[7,6,5,4,1]
  --}


--h 
solveTriangular::(Fractional a)=>[[a]]->[a]
solveTriangular (x:[]) = [(last x)/(head x)]
solveTriangular (x:xs) = (solveLine x (solveTriangular xs)):solveTriangular xs
{--
solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
[1.0,1.0,1.0]
--}

--i
solveSystem::(Fractional a,Eq a)=>[[a]]->[a]
solveSystem list = solveTriangular (triangulate list)
{--
 solveSystem [[2,5,-9,3,151],[5,6,-4,2,103],[3,-4,2,7,16],[11,7,4,-8,-32]]
 [3.0,5.0,-10.999999999999996,7.000000000000008]
--}

data MPoly = Const Rational | ProdPlus MPoly Kernel MPoly deriving (Eq,Show)
data Kernel = KVar String deriving Eq


instance Show Kernel where
  show (KVar s) = s

instance Ord Kernel where
  compare (KVar x) (KVar y) = compare x y


fromExp :: Exp -> MPoly
fromExp (RExp n)   = (Const n)
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = (fromExp u) + (fromExp v)
fromExp (Prod u v) = (fromExp u) * (fromExp v)

{--
fromExp (RExp 2)
Const (2 % 1)
--}
fromVar :: String -> MPoly
fromVar x = (ProdPlus (Const 1) (KVar x) (Const 0))

{--
 fromVar "x"
ProdPlus (Const (1 % 1)) x (Const (0 % 1))
--}

intEval :: Exp -> Rational
intEval (RExp n) = n
intEval (Var x) = error "Variable encountered in integer expression"
intEval (Sum u v) = (intEval u) + (intEval v)
intEval (Prod u v) = (intEval u) * (intEval v)
{-- intEval (RExp 3)
3 % 1
--}

instance Num MPoly where
  (Const a) + (Const b) = Const (a+b)
  (ProdPlus p1 x p2) + (Const a) = ProdPlus p1 x ((Const a) + p2)
  (Const a) + (ProdPlus p1 x p2) = ProdPlus p1 x ((Const a) + p2)
  p@(ProdPlus p1 x p2) + p'@(ProdPlus p1' y p2') 
     | x < y     = ProdPlus p1 x (p2+p')
     | x > y     = ProdPlus p1' y (p+p2')
     | otherwise = normalPoly (p1 + p1') x (p2+p2')

  negate p = scale' (-1) p

  (Const a) * p          = scale' a p
  (ProdPlus p1 x p2) * p = (p1 * (x `mulVar` p)) + p2*p

  abs _ = error "abs not supported for type MPoly"
  signum _ = error "signum not supported for type MPoly"

  fromInteger = fromConst.fromInteger
  

fromConst :: Rational -> MPoly
fromConst a = Const a
{--
 fromConst 3
Const (3 % 1)
--}

mulVar :: Kernel -> MPoly -> MPoly 
mulVar x (Const 0)   = (Const 0)
mulVar x p@(Const a) = (ProdPlus p x (Const 0))
mulVar y p@(ProdPlus p1 x p2)
  | x < y            = (ProdPlus (mulVar y p1) x (mulVar y p2))
  | x > y            = (ProdPlus p y (Const 0))
  | otherwise        = (ProdPlus p x (Const 0))
{--
 mulVar (KVar "x") (Const (3 % 1))
ProdPlus (Const (3 % 1)) x (Const (0 % 1))
--}

normalPoly :: MPoly -> Kernel -> MPoly -> MPoly
normalPoly (Const 0) x p2 = p2
normalPoly p1 x p2        = ProdPlus p1 x p2

scale' :: Rational -> MPoly -> MPoly
scale' 0 p                  = Const 0
scale' a (Const b)          = Const (a*b)
scale' a (ProdPlus p1 x p2) = ProdPlus (scale' a p1) x (scale' a p2)
{-- scale' 2  (Const (3 % 1))
Const (6 % 1)--}


listOfEquations::[Binding]->[[(String,Rational)]]
listOfEquations [] = []
listOfEquations (x:xs) = (parseEqn x):(listOfEquations xs)

parseEqn::Binding->[(String,Rational)]
parseEqn (Eqn u v) = (parsefromProdPoly (fromExp u) (fromExp v))  

parsefromProdPoly::MPoly->MPoly->[(String,Rational)]
parsefromProdPoly (ProdPlus (Const n1) (KVar x) (Const n2)) (Const n3) = [(x,n1)]++[(" ",n3-n2)]

--parsefromProdPoly (Const n3) (ProdPlus (Const n1) (KVar x) (Const n2))  = [(x,-n1)]++[(" ",n2-n3)]

parsefromProdPoly (ProdPlus (Const n1) (KVar x) (Const n2)) (ProdPlus (Const m1) (KVar y) (Const m2))
                    | x==y        = [(x,(n1-m1))]++[(" ", n2-m2)]
                    | otherwise   = [(x,n1)]++[(y,m1)]++[(" ",(m2-n2))] 
parsefromProdPoly (ProdPlus (Const n1) (KVar y) x) z  = (y,n1):(parsefromProdPoly x z)


getAllVarsfromListofVars::[Binding]->[String]
getAllVarsfromListofVars [] = []
getAllVarsfromListofVars(x:xs) = sort (nub(getVars(x)++(getAllVarsfromListofVars xs)))

getVars::Binding->[String]
getVars (Eqn u v) = getVariablesfrom(fromExp u) ++ getVariablesfrom(fromExp v)
{--
getVars (Eqn (Prod (RExp 2) (Var "y")) (RExp 10))
["y"]
--}

getVariablesfrom::MPoly->[String]
getVariablesfrom (Const n1) =[]
getVariablesfrom (ProdPlus (Const n1) (KVar x) (Const n2) ) = [x]
getVariablesfrom (ProdPlus (Const n1) (KVar x) u) =  (x):(getVariablesfrom u) 
{--
 getVariablesfrom  (Const (3 % 1))
[]
--}

insertZerosInEqn::[[(String,Rational)]]->[String]->[[(String,Rational)]]
insertZerosInEqn [] list=[]
insertZerosInEqn (x:xs) list = sortAgain(([(insert' list x)]++(insertZerosInEqn xs list)))
{--insertZerosInEqn [[("x",3),("y",3)]] ["x","y","z"]
[[("x",3 % 1),("y",3 % 1),("z",0 % 1)]]--}

sortAgain::[[(String,Rational)]]->[[(String,Rational)]]
sortAgain []   =[]
sortAgain (x:xs) =  [(s' x)]++(sortAgain xs)
{-- sortAgain  [[("y",3),("x",3)]] 
[[("x",3 % 1),("y",3 % 1)]]--}

s' :: Ord a => [(a, b)] -> [(a, b)]
s' list = sortBy (\ x y -> compare (fst x) (fst y)) list
{-- s' [("y",3),("x",3)]
[("x",3),("y",3)]--}

insert'::[String]->[(String,Rational)]->[(String,Rational)]
insert' [] listOftuple = listOftuple
insert' (x:xs) listOftuple
      | (containedin x listOftuple) = insert' xs listOftuple 
      | otherwise           = (x,0):(insert' xs listOftuple)
{--
 insert' ["x","y","z"] [("x",1),("a",2)]
[("y",0 % 1),("z",0 % 1),("x",1 % 1),("a",2 % 1)]

--}


containedin::String->[(String,Rational)]->Bool
containedin x [] = False
containedin x (y:ys) =  (check x y) || (containedin x ys)
{---
 containedin "x" [("x",2),("y",3)]
True
--}

check :: Eq a => a -> (a, b) -> Bool
check x y = if((fst y)==x) then True else False
{--
check 2 (2,3)
True
--}           

system::[Binding]->[[Rational]]
system [] =[]
system lst
  |((length (getAllVarsfromListofVars lst) ))/=(length lst) = error "Equation not solvable"
  |otherwise= nonZeroFirst (extractRat((insertZerosInEqn (listOfEquations lst) (getAllVarsfromListofVars lst) )))
{--
system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
[[1 % 1,1 % 1,2 % 1],[0 % 1,1 % 1,5 % 1]]
--}

extractRat::[[(String,Rational)]]->[[Rational]]
extractRat [] = []
extractRat(x:xs) = [(checks x)]++(extractRat xs) 
{--
 extractRat [[("x",1),("y",3)],[("r",4)]]
[[3 % 1,1 % 1],[4 % 1]]
--}


checks::([(String,Rational)])->[Rational]
checks(x:xs) = ((checker xs)++[snd x])
{--
 checks[("x",1),("y",2),("z",4)]
[2 % 1,4 % 1,1 % 1]ss
--}

checker::[(String,Rational)]->[Rational]
checker []= []
checker (y:ys) = (snd y):(checker ys)
{--
 checker [(" ",2),("x",3),("y",2)]
[2 % 1,3 % 1,2 % 1]
--}


inputEquations = do
                  putStr "Enter comma seperated equations"
                  equation<-getLine

                  case parse(tokenStreamFromString equation) of
                   (Seq a)->return (solveSystem(convertToDouble(system(extractContent(parse(tokenStreamFromString equation))))))
                   (ParseEqnSeqError e) -> error e
{--
 inputEquations 
Enter comma seperated equations2*x+3=1
[(-1) % 1]
--}


convertToDouble::[[Rational]]->[[Rational]]
convertToDouble [] = []
convertToDouble (x:xs) = (toDouble x):(convertToDouble xs)

toDouble::[Rational]->[Rational]
toDouble [] = []
toDouble(x:xs) = (x):(toDouble xs)

-------------------------------
--2
data Str = EStr | ConCat Str Char  deriving Show

data NFA q sigma = NFA (q->(Maybe sigma)->[q]) q [q]  

transitionFunction:: (Num a, Num t, Eq a) => a -> Maybe Char -> [t]
transitionFunction a b 
                    |a==0 && b  == Just 'a' = [1,2]
                    |a==0 && b  == Just 'b' = []
                    |a==0 && b  == Nothing =  [0,3]

                    |a==1 && b  == Just 'a' = []
                    |a==1 && b  == Just 'b' = [3]
                    |a==1 && b  == Nothing =  [1,2]
                    
                    |a==2 && b  == Just 'a' = []
                    |a==2 && b  == Just 'b' = []
                    |a==2 && b  == Nothing  = [2]

                    |a==3 && b  == Just 'a' = []
                    |a==3 && b  == Just 'b' = [2]
                    |a==3 && b  == Nothing  = [3]

transitionFunction _ _ = []
{--
transitionFunction 0 (Just 'a')
[1,2]
--}

nfaFromRegExp regX    = let 
          (tup,num)   = let startState=0
                    in helper1' regX startState
          xFunction l states = case lookup (l,states) tup  of
                Just char  -> char
                Nothing -> []
          in (NFA xFunction n [num])
          where n = 0


epsilonClosure :: Ord t => NFA t t1 -> [t] -> [t]
epsilonClosure _  []  = []
epsilonClosure (NFA func q sigma) lst@(x:xs)= issame (NFA func q sigma) lst (nub ([x]++(func x Nothing) ++ (epsilonClosure (NFA func q sigma) xs)))
{--
epsilonClosure (NFA transitionFunction 0 a) [0] = [0,3]
--}

issame :: Ord a => NFA a t1 -> [a] -> [a] -> [a]
issame (NFA func q sigma) list1 list2 = if (sort list1) == (sort list2) then list1 else (epsilonClosure (NFA func q sigma) list2)


helper :: Eq t3 => [t3] -> NFA t3 t2 -> t -> t1 -> t2 -> [t3]
helper [] (NFA func strtState finalstates) state string char = []
helper (l:ls) (NFA func strtState finalstates) state string char = union (func (l) (Just char)) (helper (ls) (NFA func strtState finalstates) state string char)

deltaStar :: (Ord t, Num t) => NFA t Char -> t -> Str -> [t]
deltaStar (NFA func strtState finalstates) = 
                                        let deltaStarFunction state EStr = epsilonClosure (NFA func strtState finalstates) [state]
                                            deltaStarFunction state (ConCat string char)  = epsilonClosure (NFA func strtState finalstates) (helper (deltaStarFunction strtState string) (NFA func strtState finalstates) state string char )
                                        in deltaStarFunction 



convertToStr::[Char]->Str
convertToStr [] = EStr
convertToStr [char] = ConCat EStr char 
convertToStr list = ConCat (convertToStr (init list)) (last list)
{--
 convertToStr "abc"
ConCat (ConCat (ConCat EStr 'a') 'b') 'c'
--}


doesAccept :: (Ord a, Num a) => NFA a Char -> [Char] -> Bool
doesAccept (NFA func strtState finalstates) string = intersect (finalstates) (deltaStar (NFA func strtState finalstates) strtState (convertToStr string)) /= []

{--
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "aba"
False

doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "ab"
True

--}


data RegExp sigma= 
              RegEmpty                                                       |
              RegEpsilon                                                     |
              RegSym sigma                                                   |
              RegOr  (RegExp  (sigma)) (RegExp (sigma))                      |
              RegSeq (RegExp  (sigma)) (RegExp (sigma))                      |
              RegStar (RegExp ((sigma))) deriving (Show,Eq)


--Using the NFA in the handout
helper1' RegEmpty counter = ([((counter,Nothing),[])],counter+1) 

helper1' RegEpsilon counter = ([((counter,Nothing),[counter])],counter)

helper1' (RegSym char) counter = ([((counter,(Just char)),[counter+1])], counter+1)

helper1' (RegOr exp1 exp2) counter = 
                                          let (x,y) = helper1' exp1 (counter+1)
                                              (p,v) = helper1' exp2 (y+1)
                                            in (([((counter,Nothing),[(counter+1),(y+1)])])++([((counter,Nothing),[y+1])])++([((y,Nothing),[v+1])])++([((v,Nothing),[v+1])])++x++p, v+1)


helper1' (RegSeq exp1 exp2) counter = 
                                            let (x,y) = helper1' exp1 (counter)
                                                (p,v) = helper1' exp2 (y+1)
                                              in ([((y,Nothing),[(y+1)])]++x++p,v)

helper1' (RegStar exp1) counter = let (x,y) = helper1' exp1 (counter+1)
                                          in ([((counter,Nothing),[(counter+1),(y+1)]),((y,Nothing),[(y+1),(counter+1)])]++x,(y+1)) 
{---
helper1' (RegSym 'a') 0
([((0,Just 'a'),[1])],1)

 helper1' (RegEpsilon) 0
([((0,Nothing),[0])],0)

--}












































