module BigNumber (BigNumber,scanner,output,somaBN,subBN) where


--aux functions


{-|
    Returns true if xhs is bigger than ys.(unsigned only)
-}
bigger :: BigNumber -> BigNumber -> Bool
bigger [] [] = False
bigger xs ys | head xs == head ys = bigger (tail xs) (tail ys)
             | head xs >= head ys = True
             | head xs < head ys = False
             | length xs < length ys = False
             | length xs > length ys = True

{-
    Returns true if a BigNumber is zero
-}
zero :: BigNumber -> Bool
zero [0] = True
zero xs | head xs /= 0 = False
        | otherwise    = zero (tail xs)


{-|
    Makes xs and ys the same size.
-}
normalizeLength :: BigNumber -> BigNumber -> (BigNumber,BigNumber)
normalizeLength xs ys = (addZero dif xs, addZero (-dif) ys)
    where
        dif = length ys - length xs
        addZero dif ps = replicate dif 0 ++ ps


{-|
    Auxiliary function to add two numbers without taking in account their size or signals.
-}
addCarry :: BigNumber -> BigNumber -> Int -> BigNumber
addCarry [] [] carry = [carry | carry /= 0]
addCarry xs ys carry = mod (firstX + firstY + carry) 10 : addCarry (tail xs) (tail ys) (div (firstX + firstY + carry) 10)
    where
        firstX = head xs
        firstY = head ys


{-|
    Auxiliary function to subtract two numbers without taking in account their size or signals. Xs must be bigger than ys.
-}
subAux :: BigNumber -> BigNumber -> Int -> BigNumber
subAux [] [] carry = [carry | carry /= 0]
subAux xs ys carry | firstX + carry < firstY = (firstX - firstY + carry) + 10  : subAux (tail xs) (tail ys) (-1)
                   | otherwise = firstX - firstY + carry : subAux (tail xs) (tail ys) 0
    where
        firstX = head xs
        firstY = head ys


{-|
    Auxiliary function to multiply a BigInt by one scalar, must be used in context with mulAux.
-}
getFactor :: BigNumber -> Int -> Int -> BigNumber
getFactor [] y carry = [carry | carry /= 0]
getFactor xs y carry = mod (y * head xs + carry) 10 : getFactor (tail xs) y (div (y * head xs + carry) 10)


{-|
    Auxiliary function to multiply two numbers without taking in account their size or signals. 
-}
mulAux :: BigNumber -> BigNumber -> Int -> [BigNumber]
mulAux xs [] offset = []
mulAux xs ys offset = reverse (replicate offset 0 ++ getFactor (reverse xs) (head ys) 0 ) : (mulAux xs (tail ys) (offset - 1))

{-|
    Auxiliary function to divide two numbers.
-}
--Dividend -> Divisor -> Quocient -> Reminder
divAux :: BigNumber -> BigNumber -> BigNumber -> (BigNumber,BigNumber)
divAux xs ys quocient | bigger ys xs = (quocient, xs)
                      | otherwise = divAux (0:tail (subBN xs ys)) ys (somaBN quocient [0,1])


--Exercises


--2.1
{-|
    First digit dictates the signal, whereas 0 being positve and 1 negative.
-}
type BigNumber = [Int]


--2.2
{-|
    Transforms a string in a BigNumber.
-}
scanner :: String->BigNumber
scanner = map (\x -> read [x] :: Int)


--2.3
{-|
    Transforms a BigNumber in a String.
-}
output :: BigNumber->String
output = concatMap show


--2.4
{-|
    Performs addition of two numbers.
-}
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN x' y' | negativeX /= (negativeY) = if bigger xs ys then head x' : reverse (subAux (reverse xs) (reverse ys) 0)  else head y' : reverse (subAux (reverse ys) (reverse xs) 0)
             | otherwise = (head x') : reverse (addCarry (reverse xs) (reverse ys) 0)
    where
       (xs,ys) = normalizeLength (tail x') (tail y')
       negativeX = head x' == 1
       negativeY = head y' == 1


--2.5
{-|
    Performs subtraction of two numbers.
-}
subBN :: BigNumber -> BigNumber -> BigNumber
subBN x' y' | not sameSignal = head x' : reverse (addCarry (reverse xs) (reverse ys) 0) 
            | otherwise = if bigger xs ys then head x' : reverse (subAux (reverse xs) (reverse ys) 0) else (if head y' == 1 then 0 else 1) : reverse (subAux (reverse ys) (reverse xs) 0)
    where
        (xs,ys) = normalizeLength (tail x') (tail y')
        sameSignal = head x' == head y'
        negative = (head x' == 1) || (head y' == 1)


--2.6
{-|
    Performs multiplication of two numbers.
-}
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN x' y' = signal : tail(foldr somaBN [0,0] (map (\xs->[0] ++ xs) (map (dropWhile (\x-> x == 0)) listOfFactors)))
    where
        listOfFactors = mulAux xs ys (length xs - 1) 
        (xs,ys) = normalizeLength (tail x') (tail y')
        signal = if head x' /= head y' then 1 else 0

{-|
    Performs divison of two numbers.
-}
--2.7
divBN :: BigNumber -> BigNumber -> (BigNumber,BigNumber)
divBN x' y' = divAux (0:xs) (0:ys) [0,0]
    where
        (xs,ys) = normalizeLength (tail x') (tail y')

{-|
    Performs a safe divison of two numbers
-}
safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN x' y' = if zero (tail y') then Nothing else Just (divBN x' y')