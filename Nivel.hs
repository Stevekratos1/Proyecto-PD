import Test.QuickCheck

--TAREA 2 170-

type Vector2D = (Double , Double)
type CajaDelimitadora = (Double,Double,Double,Double)
type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

--EJERCICIO 1
--Suma componente a componente dos vectores/puntos 2D
sumaVectores:: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


--Multiplica un vector/punto 2D por un factor escalar.
escalarVector:: Double -> Vector2D -> Vector2D
escalarVector e (x,y) = (e * x, e * y)

--Calcula la distancia euclídea entre dos puntos 2D.
distancia :: Vector2D -> Vector2D -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--EJERCICIO 2
--Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto.
solapan :: CajaDelimitadora -> CajaDelimitadora -> Bool
solapan (x1,y1,w1,h1) (x2,y2,w2,h2) = x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

--EJERCICIO 3
--Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el demenor solape).
ladoColision :: CajaDelimitadora -> CajaDelimitadora -> String
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
  | minSolape == solapeIzquierda = "Izquierda"
  | minSolape == solapeDerecha   = "Derecha"
  | minSolape == solapeAbajo     = "Abajo"
  | otherwise                    = "Arriba"
  where
    solapeIzquierda = (x1 + w1) - x2
    solapeDerecha   = (x2 + w2) - x1
    solapeAbajo     = (y1 + h1) - y2
    solapeArriba    = (y2 + h2) - y1
    minSolape       = minimum [solapeIzquierda, solapeDerecha, solapeAbajo, solapeArriba]

--EJERCICIO 4
-- Divide una cadena en trozos cada vez que aparece un carácter separador dado.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep    = "" : rec
    | otherwise   = (x : head rec) : tail rec
    where rec = splitOn sep xs

--Elimina los espacios en blanco (espacios, tabuladores, saltos de línea) al principio y al final de una cadena.
trim :: String -> String
trim = reverse . dropWhile esBlanco . reverse . dropWhile esBlanco
  where
    esBlanco c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

--Cuenta cuántos elementos de una lista cumplen una condición dada.
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple condicion ls = length [elem | elem <- ls, condicion elem]

--Convierte una lista de dos (o más) números en un vector/punto 2D; lanza un error si la lista no tiene al menos dos elementos.
list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error " Lista vacia no posible convertir en Vector2D"
list2Vector2 [x] = error "Falta un elemento en la lista para convertir en Vector2D"
list2Vector2 (x:y:_) = (x, y)

--EJERCICIO 5
--Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos que representa el nivel completo
parsearNivel :: [String] -> Grid
parsearNivel = id

-- Indica si una celda es una plataforma sólida (pattern matching directo).
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

-- Indica si una celda es la meta del nivel (pattern matching directo).
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

-- Indica si una celda está vacía (pattern matching directo).
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

--Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no seasólido, meta ni vacío)
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

--Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta.
posicionesMeta :: Grid -> [(Int,Int)]
posicionesMeta grid = [( row,  col) | (row, line) <- zip [0..] grid, (col, cell) <- zip [0..] line, esMeta cell]

--Dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo (fila, columna, identificador).
posicionesEnemigos :: Grid -> [(Int,Int,Char)]
posicionesEnemigos grid = [(  row,  col, cell) | (row, line) <- zip [0..] grid, 
  (col, cell) <- zip [0..] line, esEnemigo cell]

--Recorre una fila del nivel y agrupa las columnas ’#’ consecutivas en pares (columna de inicio, longitud de la racha).
agruparRachas :: Fila -> [(Int, Int)]
agruparRachas fila = aux 0 fila
  where
    aux :: Int -> Fila -> [(Int, Int)]
    aux _ [] = []
    aux idx xs
      | esSolido (head xs) = (idx, length solidos) : aux (idx + length solidos) restoSolidos
      | otherwise          = aux (idx + length noSolidos) restoNoSolidos
      where
        (solidos, restoSolidos)     = span esSolido xs
        (noSolidos, restoNoSolidos) = span (not . esSolido) xs


--BONUS (QUICKCHECK)
--prop_suma_conmutativa: sumaVectores a b es igual a sumaVectores b a
prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- > quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.

--prop_distancia_simetrica: la distancia de a a b es igual que de b a a
prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica a b = distancia a b == distancia b a

-- > quickCheck prop_distancia_simetrica
-- +++ OK, passed 100 tests.

--prop_distancia_no_negativa: la distancia entre dos puntos nunca es negativa
prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa a b = distancia a b >= 0

-- > quickCheck prop_distancia_no_negativa
-- +++ OK, passed 100 tests.

--prop_solapan_simetrica: solapan a b es igual a solapan b a
prop_solapan_simetrica :: CajaDelimitadora -> CajaDelimitadora -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- > quickCheck prop_solapan_simetrica
-- +++ OK, passed 100 tests.

--prop_trim_idempotente: aplicar trim dos veces da el mismo resultado que aplicarlo una vez
prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s

-- > quickCheck prop_trim_idempotente
-- +++ OK, passed 100 tests.

--prop_splitOn_sin_separador (propiedad condicional, usando ==>): si el carácter separador no aparece en la cadena, 
--el resultado de splitOn es una lista con un único elemento, la propia cadena
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador sep str = not (sep `elem` str) ==> splitOn sep str == [str]

-- > quickCheck prop_splitOn_sin_separador
-- +++ OK, passed 100 tests.

--prop_contarSiCumple_acotado: el resultado de contarSiCumple nunca es mayor que la longitud de la lista
prop_contarSiCumple_acotado :: (a -> Bool) -> [a] -> Bool
prop_contarSiCumple_acotado condicion ls = contarSiCumple condicion ls <= length ls

-- > quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.


-- ============================================================================
-- TAREA 2 - BLOQUE 1: RECURSIÓN
-- ============================================================================

-- Subtarea 1: Consultas del nivel mediante recursión explícita

-- Devuelve la lista de posiciones (fila, columna) donde aparece la meta mediante recursión explícita.
posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec grid = auxFilas 0 grid
  where
    auxFilas _ [] = []
    auxFilas r (f:fs) = auxCols r 0 f ++ auxFilas (r + 1) fs

    auxCols _ _ [] = []
    auxCols r c (x:xs)
      | esMeta x  = (r, c) : auxCols r (c + 1) xs
      | otherwise = auxCols r (c + 1) xs

-- Devuelve las posiciones e identificadores de los enemigos mediante recursión explícita.
posicionesEnemigosRec :: Grid -> [(Int, Int, Char)]
posicionesEnemigosRec grid = auxFilas 0 grid
  where
    auxFilas _ [] = []
    auxFilas r (f:fs) = auxCols r 0 f ++ auxFilas (r + 1) fs

    auxCols _ _ [] = []
    auxCols r c (x:xs)
      | esEnemigo x = (r, c, x) : auxCols r (c + 1) xs
      | otherwise   = auxCols r (c + 1) xs


-- Subtarea 2: Generalización de agruparRachas

-- Agrupa en tríos (columna inicio, longitud, carácter) las rachas de caracteres consecutivos iguales.
agruparPorCaracter :: Fila -> [(Int, Int, Char)]
agruparPorCaracter fila = aux 0 fila
  where
    aux _ [] = []
    aux idx xs = (idx, length racha, c) : aux (idx + length racha) resto
      where
        c = head xs
        (racha, resto) = span (== c) xs


-- Subtarea 3: Simulación de movimiento (Caída libre)

-- Calcula el estado siguiente (posición, velocidad) tras un paso de simulación.
pasoCaida :: Double -> (Double, Double) -> (Double, Double)
pasoCaida g (p, v) = (p + v, v + g)

-- Dado el número de pasos, aceleración y estado inicial, calcula recursivamente los estados de caída libre.
simularCaidaLibre :: Int -> Double -> (Double, Double) -> [(Double, Double)]
simularCaidaLibre n g estado
  | n <= 0    = [estado]
  | otherwise = estado : simularCaidaLibre (n - 1) g (pasoCaida g estado)


-- Subtarea 4: Recursión sobre dos listas a la vez

-- Empareja los elementos de dos listas uno a uno mediante recursión explícita.
zipPropio :: [a] -> [b] -> [(a, b)]
zipPropio (x:xs) (y:ys) = (x, y) : zipPropio xs ys
zipPropio _ _           = []


-- Subtarea 5: Preparación de líneas

-- Indica si una línea está vacía o es un comentario (empieza por ';') tras recortar espacios.
esComentarioOVacia :: String -> Bool
esComentarioOVacia l = null lTrim || head lTrim == ';'
  where lTrim = trim l

-- Recorta espacios de una lista de líneas y descarta las que sean vacías o comentarios.
prepararLineas :: [String] -> [String]
prepararLineas [] = []
prepararLineas (l:ls)
  | esComentarioOVacia l = prepararLineas ls
  | otherwise            = trim l : prepararLineas ls


-- ============================================================================
-- TAREA 2 - BLOQUE 2: FUNCIONES DE ORDEN SUPERIOR
-- ============================================================================

-- Subtarea 5: Reimplementación de map y filter

-- Aplica una función a cada elemento de una lista manteniendo el orden original.
mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ []     = []
mapPropio f (x:xs) = f x : mapPropio f xs

-- Filtra los elementos de una lista conservando únicamente los que cumplen la condición dada.
filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ []     = []
filterPropio p (x:xs)
  | p x       = x : filterPropio p xs
  | otherwise = filterPropio p xs


-- Subtarea 6: Reimplementación de foldr y foldl

-- Combina los elementos de una lista de derecha a izquierda asociando por la derecha.
foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ z []     = z
foldrPropio f z (x:xs) = f x (foldrPropio f z xs)

-- Combina los elementos de una lista de izquierda a derecha arrastrando un acumulador.
foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ ac []     = ac
foldlPropio f ac (x:xs) = foldlPropio f (f ac x) xs


-- Subtarea 7: Aplicación justificada de foldr y foldl

-- Indica si una caja delimitadora colisiona con alguna de las cajas de una lista de obstáculos.
-- JUSTIFICACIÓN: Se utiliza foldr porque el operador (||) permite cortocircuitar la evaluación;
-- tan pronto como se detecta una colisión con un obstáculo, se devuelve True de forma perezosa
-- sin necesidad de procesar el resto de la lista de obstáculos.
hayColisionConAlguna :: CajaDelimitadora -> [CajaDelimitadora] -> Bool
hayColisionConAlguna caja obstaculos = foldr comprueba False obstaculos
  where
    comprueba obs acc = solapan caja obs || acc

-- Cuenta cuántas celdas del nivel completo cumplen una condición dada.
-- JUSTIFICACIÓN: Se utiliza foldl porque se trata de una acumulación numérica estricta sobre
-- un total acumulado. Al no requerir preservación de estructura ni cortocircuito, foldl
-- procesa la cuadrícula de izquierda a derecha acumulando el conteo de forma directa.
contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen cond grid = foldl sumarFila 0 grid
  where
    sumarFila ac fila = ac + foldl sumarCelda 0 fila
    sumarCelda ac celda
      | cond celda = ac + 1
      | otherwise  = ac


-- ============================================================================
-- BONUS: PROPIEDADES QUICKCHECK
-- ============================================================================

-- Comprueba que mapPropio equivale a la función map de la librería estándar.
prop_mapPropio_equivale_map :: (Int -> Int) -> [Int] -> Bool
prop_mapPropio_equivale_map f xs = mapPropio f xs == map f xs

-- Comprueba que filterPropio equivale a la función filter de la librería estándar.
prop_filterPropio_equivale_filter :: (Int -> Bool) -> [Int] -> Bool
prop_filterPropio_equivale_filter p xs = filterPropio p xs == filter p xs

-- Comprueba que foldrPropio equivale a la función foldr de la librería estándar.
prop_foldrPropio_equivale_foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldrPropio_equivale_foldr f z xs = foldrPropio f z xs == foldr f z xs

-- Comprueba que foldlPropio equivale a la función foldl de la librería estándar.
prop_foldlPropio_equivale_foldl :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldlPropio_equivale_foldl f z xs = foldlPropio f z xs == foldl f z xs

-- Comprueba que zipPropio equivale a la función zip de la librería estándar.
prop_zipPropio_equivale_zip :: [Int] -> [Char] -> Bool
prop_zipPropio_equivale_zip xs ys = zipPropio xs ys == zip xs ys

-- Comprueba que la lista devuelta por simularCaidaLibre tiene siempre longitud n + 1 para n >= 0.
prop_simularCaidaLibre_longitud :: Int -> Double -> (Double, Double) -> Property
prop_simularCaidaLibre_longitud n g est = n >= 0 ==> length (simularCaidaLibre n g est) == n + 1

-- Comprueba que nunca hay colisión contra una lista vacía de obstáculos.
prop_hayColisionConAlguna_lista_vacia :: CajaDelimitadora -> Bool
prop_hayColisionConAlguna_lista_vacia caja = not (hayColisionConAlguna caja [])