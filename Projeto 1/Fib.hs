import BigNumber
--1.1
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-1) + fibRec(n-2)


--1.2
fibLista :: Int -> Int
fibLista n = lista !! n
    where
        lista = 0:1:[lista !! (x-1) + lista !! (x-2) | x<-[2..n]]


--1.3
fibListaInfinita :: Int -> Int
fibListaInfinita n = lista !! n
    where
        lista = 0:1:zipWith(+) lista (tail lista)


--3
fibRecBN :: Int -> BigNumber
fibRecBN 0 = [0,0]
fibRecBN 1 = [0,1]
fibRecBN x = somaBN (fibRecBN (x-1)) (fibRecBN(x-2))


fibListaBN :: Int -> BigNumber
fibListaBN x = last lista
    where
        lista = [0,0] : [0,1] : [somaBN (lista !! (y - 1)) (lista !! (y - 2)) | y<-[2..x]]

fibListaInfinitaBN :: Int -> BigNumber
fibListaInfinitaBN x = lista !! x
    where lista = [0,0] : [0,1] : zipWith somaBN lista (tail lista)