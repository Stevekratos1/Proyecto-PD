import Test.QuickCheck

------------------------------------------------------------------------------------------------------

-- PUNTO 5: Un informe individual donde se detalle la postura personal frente a la elección de la
-- solución final del grupo, justificando los puntos a favor o en contra de las decisiones
-- tomadas.

------------------------------------------------------------------------------------------------------

type Vector2D = (Double, Double)
type FactorEscalar = Double
type CajaDel = (Double, Double, Double, Double)
type Lado = String
type Celda = Char
type Grid  = [String]

------------------------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------------------

-- CAJAS DE COLISIÓN (2 puntos)

-- FUNCIÓN: solapan -> Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto.

solapan :: CajaDel -> CajaDel -> Bool
solapan (x1, y1, b1, h1) (x2, y2, b2, h2) = x2 < x1 + b1 && y2 < y1 + h1 && x2 + b2 > x1 && y2 + h2 > y1

---------------------------------------------------------------------------------

-- LADO DE COLISIÓN (1.5 puntos)

-- Dadas dos cajas que ya colisionan, se pide determinar por qué lado es más superficial el
-- solape (ese es el lado por el que efectivamente se produjo el contacto).

-- FUNCIÓN: ladoColision -> Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto 
-- (el de menor solape)

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

-------------------------------------------------------------------------------------------------------------

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

-- FUNCIÓN: list2Vector2 -> Convierte una lista de dos (o más) números en un vector/punto 2D; lanza un error si la lista
-- no tiene al menos dos elementos.

list2Vector2 :: [Double] -> Vector2D
list2Vector2 []        = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [_]       = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_)   = (x, y)

---------------------------------------------------------------------------------------------------------------------------

-- PARSEO DEL NIVEL (3 puntos)

-- FUNCIÓN: parsearNivel -> Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos
-- que representa el nivel completo.

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

-- FUNCIÓN: esEnemigo -> Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea
-- sólido, meta ni vacío).

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

-- FUNCIÓN: posicionesMeta -> Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la
-- meta.

posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta grid = 
  [ (fila, columna) 
  | (fila, texto) <- zip [0..] grid
  , (columna, celda) <- zip [0..] texto
  , esMeta celda 
  ]

-- FUNCIÓN: posicionesEnemigos -> Dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo
-- (fila, columna, identificador)

posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos grid = 
  [ (fila, columna, celda) 
  | (fila, texto) <- zip [0..] grid
  , (columna, celda) <- zip [0..] texto
  , esEnemigo celda
  ]

-- FUNCIÓN: agruparRachas -> Recorre una fila del nivel y agrupa las columnas ’#’ consecutivas en pares (columna de inicio,
-- longitud de la racha).

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

----------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BONUS: PROPIEDADES CON QUICKCHECK

-- prop_suma_conmutativa: sumaVectores a b es igual a sumaVectores b a

prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa a b = sumaVectores a b == sumaVectores b a

-- *Main> quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.

-- prop_suma_asociativa: sumar en un orden u otro da el mismo resultado

prop_suma_asociativa :: Vector2D -> Vector2D -> Vector2D -> Bool
prop_suma_asociativa a b c = sumaVectores c (sumaVectores a b) == sumaVectores a (sumaVectores b c)

--*Main> quickCheck prop_suma_asociativa 
--*** Failed! Falsified (after 6 tests and 15 shrinks):    
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

-- prop_splitOn_sin_separador (propiedad condicional, usando ==>): si el carácter separador no aparece en la cadena,
-- el resultado de splitOn es una lista con un único elemento, la propia cadena

prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador separador cadena = not (separador `elem` cadena) ==> splitOn separador cadena == [cadena]

-- *Main> quickCheck prop_splitOn_sin_separador 
-- +++ OK, passed 100 tests; 11 discarded.

-- prop_contarSiCumple_acotado: el resultado de contarSiCumple nunca es mayor que la longitud de la lista

prop_contarSiCumple_acotado :: [Int] -> Bool
prop_contarSiCumple_acotado xs = contarSiCumple even xs <= length xs

-- *Main> quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.