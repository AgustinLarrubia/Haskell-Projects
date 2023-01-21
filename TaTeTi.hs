import System.IO
import Data.List

{- TRABAJO PRACTICO TA-TE-TI -}
-- Larrubia, Agustin.

{-Descripcion del proyecto: Juego TA-TE-TI con tablero personalizable
por el usuario. Este ingresara el tamaño al principio del juego (tablero n x n)-}

type Valor = Char --Numero o Icono ( X O )
type Tablero = [[Valor]] --[ [1,2,3], [4,5,6], [7,8,9], ]
type Pos = (Int,Int) -- (i,j)
type Lista = [Valor]
type Jugador = Int -- 1 o 2


------------ IO ------------
------- Input Output -------
{- FUNCIONES INTERACTIVAS -}


main = do mensajeBienvenida




mensajeBienvenida = do putStrLn "----------Bienvenido al Ta-Te-Ti----------"
                       putStrLn " Jugador 1 = 'X' -------- Jugador 2 = 'O' "
                       putStrLn "Ingrese el tamaño deseado para el tablero:"
                       n <- getNum
                       principal n (crearTablero n) 1
                       continuar




principal n t p = do putStrLn (columnas (length t + 1) 0)
                     mapM_ putStrLn (imprimir t (length t) (length t) 0)
                     (i, j) <- (validarMovimiento t p)
                     putStrLn "--------------------------------------"
                     t <- (mover p (i-1,j-1) t)
                     if (estado t p) then do putStrLn (columnas (length t + 1) 0)
                                             mapM_ putStrLn (imprimir t (length t) (length t) 0)
                                             putStrLn ("Gano el jugador " ++ show p ++".")

                                     else if p == 1 then principal n t 2
                                                    else principal n t 1


getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn



validarMovimiento:: Tablero -> Jugador -> IO(Int,Int)
validarMovimiento t p = do putStrLn ("Jugador " ++ show p ++". Ingrese una fila:")
                           i <- getNum
                           putStrLn ("Jugador " ++ show p ++". Ingrese una columna:")
                           j <- getNum
                           validarJugada t p i j




validarJugada:: Tablero -> Jugador -> Int -> Int -> IO(Int,Int)
validarJugada t p i j = if validarPos (i,j) t && validarUbi (i,j) t then do  return(i,j)
                                                                    else do  putStrLn "La Posicion se encuentra ocupada o fuera del tablero."
                                                                             putStrLn "Ingrese una fila y columna nuevamente:"
                                                                             putStrLn (columnas (length t + 1) 0)
                                                                             mapM_ putStrLn (imprimir t (length t) (length t) 0)
                                                                             validarMovimiento t p


continuar = do putStrLn "¿Quiere seguir jugando? 's' para seguir 'n' para salir"
               c <- getLine
               reiniciar c

mover:: Jugador -> Pos -> Tablero -> IO(Tablero)
mover p pos t = do return(ponerFicha p pos t)

leerTupla:: Int -> Int -> IO(Pos)
leerTupla i j = do return(i,j)






{- FUNCIONES PURAS -}
--crearTablero llama a la funcion tableron y reparte el n en dos parametros
--y devuelve un tablero de n listas y n caracteres
crearTablero:: Int -> Tablero
crearTablero n = tableron n n

--tableron crea una lista de n cantidad de listas de longitud nc
tableron :: Int -> Int -> Tablero
tableron _ 0 = []
tableron nc n = caracter nc : tableron nc (n-1)
{- TRAZA
tableron 4 4 = caracter 4 : tableron 4 (4-1)
                "****" :  caracter 4 : tableron 4 (3-1)
                "****" : "****" : caracter 4 : tableron 4 (2-1)
                "****" : "****" : "****" : caracter 4 : tableron 4 (1-1)
                "****" : "****" : "****" : "****" : []
                ["****","****","****","****"]
-}
-- caracter: Crea una lista de caracteres de n longitud
caracter :: Int -> [Valor]
caracter 0 = []
caracter n = '*': caracter (n-1)



-- Imprimir tablero
imprimir:: Tablero -> Int -> Int -> Int -> Tablero
imprimir [] _ _ _ = []
imprimir (x:xs) l 1 n = barras x l l (n + 1) : imprimir xs l 1 (n + 1)
imprimir (x:xs) l ls n = barras x l l (n + 1) : (' ' : ' ' : linea (4 * l - 1)) : imprimir xs l (ls - 1) (n + 1)


linea:: Int -> Lista
linea 0 = []
linea n = '-' : linea (n-1)

columnas:: Int -> Int -> Lista
columnas l n | n == 0    =  [' '] ++ [' '] ++ [' '] ++ columnas l (n + 1)
             | l == n    = []
             | otherwise = show n ++ [' '] ++ [' '] ++ [' '] ++ columnas l (n + 1)

barras:: Lista -> Int -> Int -> Int -> Lista
barras ys 0 _ _ = ys
barras (y:ys) l ls n | l == ls = show(n) ++ [':'] ++ [' '] ++ barras (y:ys) (l - 1) ls n
                     | otherwise = [y] ++ [' '] ++ ['|'] ++ [' '] ++ barras ys (l - 1) ls n




-- validarPos: Valida la posicion indicada por el jugador. Si esta fuera de la lista indica False
validarPos :: Pos -> Tablero -> Bool
validarPos (i,j) t | (i-1 >= 0 && i-1 <= length t) && (j-1 >= 0 && j-1 <= length t) = True
                   | otherwise = False


-- validarUbi: valida si la posicion esta disponible para colocar la ficha
validarUbi :: Pos -> Tablero -> Bool
validarUbi (i,j) t | i-1 == length(t) && j-1 == length(t)   = False
                   | ver(i,j) t == 'X' || ver(i,j) t == 'O' = False
                   | otherwise = True


--ver: devuelve el elemento de la Lista
ver :: Pos -> Tablero -> Valor
ver (i,j) t = (t !! (i-1)) !! (j-1)


-- ponerFicha: Inserta ficha en tablero (let in or where?)
ponerFicha :: Jugador -> Pos -> Tablero -> Tablero
ponerFicha p (0,j) (c:cs) = let caracter = case p of 1 -> 'X'
                                                     2 -> 'O'
                            in reemplazo caracter (0,j) (c:cs) : cs
ponerFicha p (i,j) (c:cs) = c : ponerFicha p ((i-1),j) cs


{- Reemplazo: Toma un valor ('X' o 'Y'), una posicion (i,j) y el tablero.
Y devuelve una lista con el valor reemplazado -}
reemplazo :: Valor -> Pos -> Tablero -> Lista
reemplazo v (i,j) cs = del v (cs !! i) j -- (cs !! i) devuelve la fila a modificar


{-del coloca el valor en la lista indicada (cs !! i) -}
del :: Valor -> Lista -> Int -> Lista
del v (_:cs) 0 = v : cs
del v (c:cs) j = c : (del v cs (j-1))

--reiniciar:: Valor ->
reiniciar c | c == "s" || c == "S" = mensajeBienvenida
            | c == "n" || c == "N" = return("Gracias por jugar")
            |otherwise = continuar

{-estado:: Tablero -> Jugador -> Bool
estado recibe un tablero n x n y si existe una linea vertical, horizontal o diagonal
de tamanio n, devuelve un valor de verdad. Estado reune a 3 funciones. diagonal vertical y horizontal-}
estado::Tablero -> Jugador -> Bool
estado (c:cs) j = diagonal (c:cs) j || vertical (c:cs) j || horizontal (c:cs) j


{-horizontal:: Tablero -> Bool-}
horizontal:: Tablero -> Jugador -> Bool
horizontal (c:cs) j = verHor (c:cs) (length(c)) j --length(c) = n

{-vertical:: Tablero -> Jugador -> Bool
vertical recibe un tablero y recorre columnas y filas, Si encuentra TaTeTi devuelve TRUE-}
vertical:: Tablero -> Jugador -> Bool
vertical (c:cs) j = verHor (transpose(c:cs)) (length (c)) j

verHor:: Tablero -> Int -> Jugador -> Bool
verHor (c:cs) 1 j = foldr1 (&&) (map(== caracter) c)
      where
        caracter = case j of
          1 -> 'X'
          2 -> 'O'
verHor (c:cs) n j = foldr1 (&&) (map(== caracter) c) || verHor cs (n - 1) j
      where
        caracter = case j of
          1 -> 'X'
          2 -> 'O'


diagonal:: Tablero -> Jugador -> Bool
diagonal c j = foldr1 (&&) (map(== caracter) (derizqDiag c (length(c)))) || foldr1 (&&) (map(== caracter) (izqderDiag c (length(c)) (length(c))))
        where
          caracter = case j of
            1 -> 'X'
            2 -> 'O'


izqderDiag:: Tablero -> Int -> Int -> Lista
izqderDiag _ _ 0 = []
izqderDiag (c:cs) n m = c !! (n - m) : izqderDiag cs n (m - 1)

derizqDiag:: Tablero -> Int -> Lista
derizqDiag _ 0 = []
derizqDiag (c:cs) n = c !! (n - 1) : derizqDiag cs (n - 1)
