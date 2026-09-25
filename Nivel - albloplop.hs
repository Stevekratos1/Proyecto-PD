type Vector2 = (Float, Float)
type Caja = (Float, Float, Float, Float)
data Lado = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)
type Celda = Char
type Fila = String
type Nivel = [Fila]

-- 1. Vectores 2D

sumaVectores :: Vector2 -> Vector2 -> Vector2
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

escalarVector :: Float -> Vector2 -> Vector2
escalarVector c (x, y) = (c * x, c * y)

distancia :: Vector2 -> Vector2 -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- 2. Cajas de colisión

solapan :: Caja -> Caja -> Bool
solapan (x1, y1, w1, h1) (x2, y2, w2, h2) =
    x1 < x2 + w2 && x1 + w1 > x2 &&
    y1 < y2 + h2 && y1 + h1 > y2

-- 3. Lado de colisión

ladoColision :: Caja -> Caja -> Lado
ladoColision (x1, y1, w1, h1) (x2, y2, w2, h2)
    | minSolape == sArriba    = Arriba
    | minSolape == sAbajo     = Abajo
    | minSolape == sIzquierda = Izquierda
    | otherwise               = Derecha
  where
    sArriba    = (y2 + h2) - y1
    sAbajo     = (y1 + h1) - y2
    sIzquierda = (x1 + w1) - x2
    sDerecha   = (x2 + w2) - x1
    minSolape  = min (min sArriba sAbajo) (min sIzquierda sDerecha)

-- 4. Utilidades de listas y cadenas

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

trim :: String -> String
trim = reverse . quitarEspacios . reverse . quitarEspacios
  where
    quitarEspacios :: String -> String
    quitarEspacios [] = []
    quitarEspacios (c:cs)
        | c == ' ' || c == '\t' || c == '\n' || c == '\r' = quitarEspacios cs
        | otherwise = c:cs

contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple cond xs = length [x | x <- xs, cond x]

list2Vector2 :: [Float] -> Vector2
list2Vector2 [] = error "Lista vacia no posible convertir en Vector2"
list2Vector2 [_] = error "Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x, y)

-- 5. Parseo del nivel

parsearNivel :: [String] -> Nivel
parsearNivel lineas = lineas

esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _   = False

esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _   = False

esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _   = False

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

posicionesMeta :: Nivel -> [(Int, Int)]
posicionesMeta nivel = 
    [(f, c) | (fila, f) <- zip nivel [0..], (celda, c) <- zip fila [0..], esMeta celda]

posicionesEnemigos :: Nivel -> [(Int, Int, Celda)]
posicionesEnemigos nivel = 
    [(f, c, celda) | (fila, f) <- zip nivel [0..], (celda, c) <- zip fila [0..], esEnemigo celda]

agruparRachas :: Fila -> [(Int, Int)]
agruparRachas fila = procesar fila 0
  where
    procesar :: Fila -> Int -> [(Int, Int)]
    procesar [] _ = []
    procesar (x:xs) idx
        | esSolido x =
            let (racha, resto) = extraerRacha (x:xs)
                len = length racha
            in (idx, len) : procesar resto (idx + len)
        | otherwise = procesar xs (idx + 1)

    extraerRacha :: Fila -> (String, String)
    extraerRacha [] = ([], [])
    extraerRacha (y:ys)
        | esSolido y = let (r, rest) = extraerRacha ys in (y:r, rest)
        | otherwise  = ([], y:ys)