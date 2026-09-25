

-- DECLARACIONES DE TIPO
type Punto  = (Double, Double)
type Caja   = (Double, Double, Double, Double)
type Vector = (Double, Double)
type Celda  = Char
type Grid   = [String]

-- EJERCICIO 1: Vectores 2D

-- Suma componente a componente dos vectores/puntos 2D
sumaVectores :: Punto -> Punto -> Punto
sumaVectores x y = (fst x + fst y, snd x + snd y)

-- Multiplica un vector/punto 2D por un factor escalar.
escalarVector :: Double -> Punto -> Punto
escalarVector x y = (fst y * x, snd y * x)

-- Calcula la distancia euclídea entre dos puntos 2D.
distancia :: Punto -> Punto -> Double
distancia x y = sqrt ((fst x - fst y)^2 + (snd x - snd y)^2)


-- EJERCICIO 2: Cajas de colisión

-- Extrae el primer componente (Posición X) de una caja.
primero :: Caja -> Double
primero (x,_,_,_) = x

-- Extrae el segundo componente (Posición Y) de una caja.
segundo :: Caja -> Double
segundo (_, y, _, _) = y

-- Extrae el tercer componente (Ancho) de una caja.
tercero :: Caja -> Double
tercero (_, _, z, _) = z

-- Extrae el cuarto componente (Alto) de una caja.
cuarto :: Caja -> Double
cuarto (_, _, _, w) = w

-- Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto..
solapan :: Caja -> Caja -> Bool
solapan x y = l1 && l2
    where 
        l1 = (primero x < primero y + tercero y) && (primero y < primero x + tercero x)
        l2 = (segundo x < segundo y + cuarto y) && (segundo y < segundo x + cuarto x)


-- EJERCICIO 3: Lado de la Colisión

-- Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el de menor solape
ladoColision :: Caja -> Caja -> String
ladoColision caja1 caja2 = lado
    where
    arriba    = abs (segundo caja2 - (segundo caja1 + cuarto caja1))
    abajo     = abs (segundo caja1 - (segundo caja2 + cuarto caja2))
    izquierda = abs (primero caja1 - (primero caja2 + tercero caja2))
    derecha   = abs (primero caja2 - (primero caja1 + tercero caja1))
    l         = [arriba, abajo, izquierda, derecha]
    m         = maximum l
    lista     = zip l ["arriba", "abajo", "izquierda", "derecha"] 
    lado      = head [ nombre | (valor, nombre) <- lista, valor == m]


-- EJERCICIO 4: Utilidades de listas y cadenas

-- Divide una cadena de texto en subcadenas utilizando un carácter separador dado.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

-- Elimina los espacios en blanco, tabuladores y saltos al inicio y al final de una cadena.
trim :: String -> String
trim = trimFinal . trimPrincipio
  where
    trimPrincipio [] = []
    trimPrincipio (x:xs)

        | x == ' ' || x == '\t' || x == '\n' || x == '\r' = trimPrincipio xs
        | otherwise                                       = x:xs
    trimFinal = reverse . trimPrincipio . reverse

-- Cuenta cuántos elementos de una lista cumplen una condición o predicado dado.
contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple condicion lista = sum [1 | x <- lista, condicion x]

-- Convierte una lista con al menos dos números Double en un tipo Vector.
list2Vector2 :: [Double] -> Vector
list2Vector2 []      = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [_]     = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)


-- EJERCICIO 5: Parseo del nivel

-- Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos que representa el nivel completo
parsearNivel lineas = lineas


-- Indica si una celda es una plataforma sólida.
esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

-- Indica si una celda es la meta del nivel
esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

-- Indica si una celda está vacía (no hay nada en ella)
esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

-- Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea sólido, meta ni vacío).
esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

-- Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la meta.
posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta grid =
    [ (i, j)
    | (i, fila) <- zip [0..] grid
    , (j, c)    <- zip [0..] fila
    , esMeta c
    ]

-- Encuentra las coordenadas de todos los enemigos junto a su carácter identificador.
posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos grid =
    [ (i, j, c)
    | (i, fila) <- zip [0..] grid
    , (j, c)    <- zip [0..] fila
    , esEnemigo c
    ]

-- Dado el nivel completo, devuelve la lista de posiciones y el identificador de cada enemigo (fila, columna, identificador)
agruparRachas :: String -> [(Int, Int)]
agruparRachas filaTexto = buscar 0 filaTexto
  where
    -- recorre la fila de texto para encontrr y medir dónde empiezan y cuánto miden las rachas continuas de bloques sólidos 
    buscar :: Int -> String -> [(Int, Int)]
    buscar _ [] = []
    buscar col xs
        | esSolido (head xs) = 
            let (solidos, resto) = span esSolido xs
                longitud = length solidos
            in (col, longitud) : buscar (col + longitud) resto
        | otherwise = 
            let (_, resto) = span (not . esSolido) xs
                noSolidos  = length xs - length resto
            in buscar (col + noSolidos) resto
