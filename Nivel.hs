import Test.QuickCheck

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--TAREA 1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

type Vector2D = (Double, Double)
type FactorEscalar = Double
type CajaDel = (Double, Double, Double, Double)
type Lado = String
type Celda = Char
type Grid  = [String]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- VECTORES 2D (1.5 puntos)

-- FUNCIÓN: sumaVectores -> Suma componente a componente dos vectores/puntos 2D.

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- FUNCIÓN: escalarVector -> Multiplica un vector/punto 2D por un factor escalar.

escalarVector :: FactorEscalar -> Vector2D -> Vector2D
escalarVector factorEsc (x, y) = (x*factorEsc, y*factorEsc)

-- FUNCIÓN: distancia -> Calcula la distancia euclídea entre dos puntos 2D.

distancia :: Vector2D -> Vector2D -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- CAJAS DE COLISIÓN (2 puntos)

-- FUNCIÓN: solapan -> Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto.

solapan :: CajaDel -> CajaDel -> Bool
solapan (x1, y1, b1, h1) (x2, y2, b2, h2) = x2 < x1 + b1 && y2 < y1 + h1 && x2 + b2 > x1 && y2 + h2 > y1

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- LADO DE COLISIÓN (1.5 puntos)

-- Dadas dos cajas que ya colisionan, se pide determinar por qué lado es más superficial el solape (ese es el lado por el que efectivamente se produjo el contacto).

-- FUNCIÓN: ladoColision -> Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el de menor solape)

ladoColision :: CajaDel -> CajaDel -> String
ladoColision (x1, y1, b1, h1) (x2, y2, b2, h2)
    | minimo == arriba = "Arriba"
    | minimo == abajo = "Abajo"
    | minimo == izq = "Izquierda"
    | minimo == der = "Derecha"
    where
        arriba = (y2 + h2) - y1 
        abajo = (y1 + h1) - y2
        izq = (x1 + b1) - x2
        der = (x2 + b2) - x1
        minimo = min (min arriba abajo) (min izq der)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- UTILIDADES DE LISTAS Y CADENAS (2 puntos)

-- FUNCIÓN: splitOn -> Divide una cadena en trozos cada vez que aparece un carácter separador dado.

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn separador (x:xs)
    | x == separador =  "" : recursión
    | otherwise = (x : head recursión) : tail recursión
    where recursión = splitOn separador xs

-- FUNCIÓN: trim -> Elimina los espacios en blanco (espacios, tabuladores, saltos de línea) al principio y al final de una cadena.

trim :: String -> String
trim = dropWhile espacio . reverse . dropWhile espacio . reverse
  where
    espacio c = c `elem` " \t\n\r"

-- FUNCIÓN: contarSiCumple -> Cuenta cuántos elementos de una lista cumplen una condición dada.

contarSiCumple :: (a-> Bool) -> [a] -> Int
contarSiCumple cond xs = length [x | x <- xs, cond x]

-- FUNCIÓN: list2Vector2 -> Convierte una lista de dos (o más) números en un vector/punto 2D; lanza un error si la lista no tiene al menos dos elementos.

list2Vector2 :: [Double] -> Vector2D
list2Vector2 []        = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [_]       = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_)   = (x, y)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- PARSEO DEL NIVEL (3 puntos)

-- FUNCIÓN: parsearNivel -> Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos que representa el nivel completo.

parsearNivel :: [String] -> Grid
parsearNivel = id

-- FUNCIÓN: esSolido -> Indica si una celda es una plataforma sólida.

esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

-- FUNCIÓN: esMeta -> Indica si una celda es la meta del nivel.

esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

-- FUNCIÓN: esVacio -> Indica si una celda está vacía (no hay nada en ella).

esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

-- FUNCIÓN: esEnemigo -> Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea sólido, meta ni vacío).

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

-- FUNCIÓN: posicionesMeta -> Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta.

posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta grid = 
  [ (fila, columna) 
  | (fila, texto) <- zip [0..] grid
  , (columna, celda) <- zip [0..] texto
  , esMeta celda 
  ]

-- FUNCIÓN: posicionesEnemigos -> Dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo (fila, columna, identificador).

posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos grid = 
  [ (fila, columna, celda) 
  | (fila, texto) <- zip [0..] grid
  , (columna, celda) <- zip [0..] texto
  , esEnemigo celda
  ]

-- FUNCIÓN: agruparRachas -> Recorre una fila del nivel y agrupa las columnas ’#’ consecutivas en pares (columna de inicio, longitud de la racha).

agruparRachas :: String -> [(Int, Int)]
agruparRachas fila = aux 0 fila
  where
    aux :: Int -> String -> [(Int, Int)]
    aux _ [] = []
    aux contador (c:cs)
      | esSolido c = 
          let (solidos, resto) = span esSolido (c:cs)
              finSolido = length solidos
          in (contador, finSolido) : aux (contador + finSolido) resto
      | otherwise  = aux (contador + 1) cs

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BONUS: PROPIEDADES CON QUICKCHECK

-- prop_suma_conmutativa: sumaVectores a b es igual a sumaVectores b a

prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- *Main> quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.

-- prop_suma_asociativa: sumar en un orden u otro da el mismo resultado

prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores c (sumaVectores a b) == sumaVectores a (sumaVectores b c)

-- *Main> quickCheck prop_suma_asociativa 
-- *** Failed! Falsified (after 6 tests and 15 shrinks):    
--(0.0,0.1)
--(0.0,0.2)
--(0.0,-1.0)

-- prop_escalar_neutro: escalar un vector por 1 no lo cambia

prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- *Main> quickCheck prop_escalar_neutro 
-- +++ OK, passed 100 tests.

-- prop_distancia_no_negativa: la distancia entre dos puntos nunca es negativa

prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa v1 v2 = distancia v1 v2 >= 0

-- *Main> quickCheck prop_distancia_no_negativa 
-- +++ OK, passed 100 tests.

-- prop_distancia_simetrica: la distancia de a a b es igual que de b a a

prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica v1 v2 = distancia v1 v2 == distancia v2 v1

-- *Main> quickCheck prop_distancia_simetrica
-- +++ OK, passed 100 tests.

-- prop_solapan_simetrica: solapan a b es igual a solapan b a

prop_solapan_simetrica :: CajaDel -> CajaDel -> Bool
prop_solapan_simetrica (x1, y1, b1, h1) (x2, y2, b2, h2) = solapan (x1, y1, b1, h1) (x2, y2, b2, h2) == solapan (x2, y2, b2, h2) (x1, y1, b1, h1) 

-- *Main> quickCheck prop_solapan_simetrica 
-- +++ OK, passed 100 tests.

-- prop_trim_idempotente: aplicar trim dos veces da el mismo resultado que aplicarlo una vez

prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- *Main> quickCheck prop_trim_idempotente
-- +++ OK, passed 100 tests.

-- prop_splitOn_sin_separador (propiedad condicional, usando ==>): si el carácter separador no aparece en la cadena, el resultado de splitOn es una lista con un 
-- único elemento, la propia cadena.

prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador separador cadena = not (separador `elem` cadena) ==> splitOn separador cadena == [cadena]

-- *Main> quickCheck prop_splitOn_sin_separador 
-- +++ OK, passed 100 tests; 11 discarded.

-- prop_contarSiCumple_acotado: el resultado de contarSiCumple nunca es mayor que la longitud de la lista.

prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado xs = contarSiCumple even xs <= length xs

-- *Main> quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--TAREA 2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 1 Reimplementación recursiva de las consultas del nivel (1.5 puntos)

-- FUNCIÓN: posicionesMetaRec -> Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta. Mismo resultado que posicionesMeta
-- de la Tarea 1, pero implementado con recursión explícita en vez de comprensión.

posicionesMetaRec:: Grid -> [(Int, Int)]
posicionesMetaRec = filaMetaRec 0

filaMetaRec :: Int -> Grid -> [(Int, Int)]
filaMetaRec _ [] = []
filaMetaRec fila (xs:xss) 
  | or (map esMeta xs) = posicionMeta fila 0 xs ++ filaMetaRec (fila + 1) xss
  | otherwise = filaMetaRec (fila + 1) xss

posicionMeta :: Int -> Int -> String -> [(Int, Int)]
posicionMeta _ _ [] = []
posicionMeta fila columna (x:xs)
  | esMeta x  = (fila, columna) : posicionMeta fila (columna + 1) xs
  | otherwise = posicionMeta fila (columna + 1) xs

-- FUNCIÓN: posicionesEnemigosRec -> Análoga a posicionesMetaRec, para las posiciones e identificadores de los enemigos.

posicionesEnemigosRec:: Grid -> [(Int, Int, Char)]
posicionesEnemigosRec = filaEnemigosRec 0

filaEnemigosRec :: Int -> Grid -> [(Int, Int, Char)]
filaEnemigosRec _ [] = []
filaEnemigosRec fila (xs:xss) 
  | or (map esEnemigo xs) = posicionEnemigo fila 0 xs ++ filaEnemigosRec (fila + 1) xss
  | otherwise             = filaEnemigosRec (fila + 1) xss

posicionEnemigo :: Int -> Int -> String -> [(Int, Int, Char)]
posicionEnemigo _ _ [] = []
posicionEnemigo fila columna (x:xs)
  | esEnemigo x = (fila, columna, x) : posicionEnemigo fila (columna + 1) xs
  | otherwise   = posicionEnemigo fila (columna + 1) xs

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 2 Generalización por recursión de agruparRachas (1.5 puntos)

-- FUNCIÓN: agruparPorCaracter ->  Recorre una fila y agrupa en tríos (columna de inicio, longitud, carácter) las rachas de caracteres consecutivos iguales.

agruparPorCaracter:: String -> [(Int, Int, Char)]
agruparPorCaracter = agruparPorCaracterAux 0 1

agruparPorCaracterAux :: Int -> Int -> String -> [(Int, Int, Char)]
agruparPorCaracterAux _ _ [] = []
agruparPorCaracterAux fila contador (x:xs) 
  | null xs      = (fila, contador, x): agruparPorCaracterAux fila (contador + 1) xs
  | x == head xs = agruparPorCaracterAux fila (contador + 1) xs
  | otherwise    = (fila, contador, x) : agruparPorCaracterAux (fila + contador) 1 xs

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 3 Recursión numérica: simulación de movimiento (1 punto)

-- Esta subtarea introduce, mediante recursión pura, la idea que en la Tarea 3 se convertirá en la física del juego: calcular cómo evoluciona una posición paso a paso.

-- FUNCIÓN: simularCaidaLibre -> Dado un número de pasos, una aceleración (gravedad) y un estado inicial (posición, velocidad), calcula recursivamente la lista de todos
-- los estados por los que pasa un objeto en caída libre con avance discreto.

-- Nota: La lista resultado tiene siempre n + 1 elementos: el estado inicial más uno por cada paso simulado.

simularCaidaLibre:: Int -> Double -> Vector2D -> [Vector2D]
simularCaidaLibre 0 _ (x, y) = (x, y): []
simularCaidaLibre n acel (x,y) = (x, y) : simularCaidaLibre (n-1) acel (x + y, y + acel)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 4 Recursión sobre dos listas a la vez (0.5 puntos)

-- FUNCIÓN: zipPropio -> Empareja los elementos de dos listas, uno a uno, deteniéndose en cuanto una de las dos se acaba.

zipPropio:: [a] -> [b] -> [(a,b)]
zipPropio [] _ = []
zipPropio _ [] = []
zipPropio (x:xs) (y:ys) = (x,y) : zipPropio xs ys

-- > zipPropio [1,2,3] "abc"
-- [(1,’a’),(2,’b’),(3,’c’)]
-- > zipPropio [1,2,3,4,5] [10,20]
-- [(1,10),(2,20)]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 5 Recursión con descarte condicional: preparación de líneas (0.5 puntos)

-- FUNCIÓN: prepararLineas -> Limpia una lista de líneas leídas de un fichero de nivel: recorta espacios en cada línea (con esComentarioOVacia para detectar comentarios, 
-- que empiezan por ’;’) y descarta las que queden vacías o sean comentarios.

-- > esComentarioOVacia ";esto es un comentario"
-- True
-- > prepararLineas [" ..M ", "", ";comentario", "#.X", " ", "###"]
-- ["..M","#.X","###"]

esComentarioOVacia:: String -> Bool
esComentarioOVacia cadena
  | null cadenaLimpia || head cadenaLimpia  == ';' = True
  | otherwise = False
  where cadenaLimpia = trim cadena

prepararLineas:: [String] -> [String]
prepararLineas [] = []
prepararLineas (xs:xss) 
  | esComentarioOVacia xs = prepararLineas xss
  | otherwise = trim xs : prepararLineas xss
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Subtarea 5: mapPropio y filterPropio

-- FUNCIÓN: mapPropio -> Aplica una función a cada elemento de una lista, devolviendo la lista de resultados en el mismo orden. No podéis usar map de la librería para 
-- definirla.

mapPropio:: (a -> b) -> [a] -> [b]
mapPropio f xs =  [f x | x <- xs]

-- FUNCIÓN: filterPropio -> Se queda con los elementos de una lista que cumplen una condición dada. No podéis usar filter de la librería para definirla.

filterPropio:: (a -> Bool) -> [a] -> [a]
filterPropio cond xs = [x | x <- xs, cond x]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Subtarea 6: foldrPropio y foldlPropio

-- FUNCIÓN: foldrPropio -> Combina los elementos de una lista de derecha a izquierda usando una función binaria y un valor inicial. No podéis usar 
-- foldr de la librería para definirla.

foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ v []     = v
foldrPropio f v (x:xs) = f x (foldrPropio f v xs)

-- FUNCIÓN: foldlPropio -> Combina los elementos de una lista de izquierda a derecha, arrastrando un acumulador que empieza en el valor inicial.
-- No podéis usar foldl de la librería para definirla.

foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ v []     = v
foldlPropio f v (x:xs) = foldlPropio f (f v x) xs

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Subtarea 7: Aplicación justificada de foldr/foldl (2.5 puntos)

-- Ahora se pide usar foldr y foldl de la librería estándar (no vuestras versiones propias) para resolver dos problemas reales del juego, eligiendo en cada caso la que
-- corresponda y justificando por escrito, en un comentario de 2–3 líneas encima de la función, por qué esa es la elección adecuada y no la otra.

-- FUNCIÓN: hayColisionConAlguna -> Dada la caja delimitadora de un objeto (por ejemplo, el jugador) y una lista de cajas de posibles obstáculos, indica si colisiona 
-- con alguna de ellas. Es el tipo de comprobación que necesitará el sistema de colisiones de la Tarea 3, cuando el jugador tenga que comprobarse contra todas las
-- plataformas o enemigos del nivel a la vez.

-- Debe implementarse con foldr.

hayColisionConAlguna:: CajaDel -> [CajaDel] -> Bool
hayColisionConAlguna caja = foldr (comprobarColision caja)  False
  where
    comprobarColision c obst acc = solapan c obst || acc


-- foldr es adecuado en este caso porque al asociar por la derecha, se comprueba el elemento actual. Esto hace que si el primer elemento solapa no se tenga que comprobar
-- el resto de la lista. foldl al acumular por la izquiera, genera una cadena anidada de operaciones que olbiga a recorrer y evaluar la lista entera.

-- FUNCIÓN: contarCeldasQueCumplen -> Cuenta cuántas celdas del nivel completo cumplen una condición dada (misma funcionalidad que el contarCeldas que se podría haber
-- pedido con comprensión, pero ahora resuelta con pliegues).

-- Debe implementarse con foldl (una función auxiliar para contar dentro de cada fila, combinada
-- con otra que suma los resultados de todas las filas).

contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Integer
contarCeldasQueCumplen cond = foldl sumarFila 0
  where
    sumarFila acc fila = acc + contarEnFila fila
    contarEnFila = foldl (contarUnaCelda cond) 0
    contarUnaCelda cond acc c 
      | cond c    = acc + 1
      | otherwise = acc

-- En este caso necesitamos recorrer la lista al completo por lo que la ventaja de foldr no es necesaria. Es por ello que utilizar foldl en este caso es lo adecuado.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Bonus: Propiedades con QuickCheck (hasta +1 punto extra)

-- FUNCIÓN: prop_mapPropio_equivale_map

prop_mapPropio_equivale_map :: [Int] -> Bool
prop_mapPropio_equivale_map xs = mapPropio (+1) xs == map (+1) xs

-- *Main> quickCheck prop_mapPropio_equivale_map
-- +++ OK, passed 100 tests.

-- FUNCIÓN: prop_filterPropio_equivale_filter

prop_filterPropio_equivale_filter:: [Int] -> Bool
prop_filterPropio_equivale_filter xs = filterPropio even xs == filter even xs

-- *Main> quickCheck prop_filterPropio_equivale_filter
-- +++ OK, passed 100 tests.
 
-- FUNCIÓN: prop_foldrPropio_equivale_foldr

prop_foldrPropio_equivale_foldr :: [Int] -> Bool
prop_foldrPropio_equivale_foldr xs = foldrPropio (+) 0 xs == foldr (+) 0 xs

-- *Main> quickCheck prop_foldrPropio_equivale_foldr
-- +++ OK, passed 100 tests.

-- FUNCIÓN: prop_foldlPropio_equivale_foldl

prop_foldlPropio_equivale_foldl :: [Int] -> Bool
prop_foldlPropio_equivale_foldl xs = foldlPropio (+) 0 xs == foldl (+) 0 xs

-- *Main> quickCheck prop_foldlPropio_equivale_foldl
-- +++ OK, passed 100 tests.

-- FUNCIÓN: prop_simularCaidaLibre_longitud (propiedad condicional, usando ==> y restringida a n >= 0)

prop_simularCaidaLibre_longitud :: Int -> Double -> Vector2D -> Property
prop_simularCaidaLibre_longitud n acel estado = n >= 0 ==> length (simularCaidaLibre n acel estado) == n + 1

-- *Main> quickCheck prop_simularCaidaLibre_longitud 
-- +++ OK, passed 100 tests; 82 discarded.

-- FUNCIÓN: prop_hayColisionConAlguna_lista_vacia: nunca hay colisión contra una lista vacía de obstáculos.

prop_hayColisionConAlguna_lista_vacia:: CajaDel -> Bool
prop_hayColisionConAlguna_lista_vacia caja = not (hayColisionConAlguna caja [])

-- *Main> quickCheck prop_hayColisionConAlguna_lista_vacia
-- +++ OK, passed 100 tests.
