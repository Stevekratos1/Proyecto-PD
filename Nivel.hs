import Test.QuickCheck


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