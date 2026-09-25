-- Ej1

type Punto = (Double, Double)

sumaVectores :: Punto -> Punto -> Punto

sumaVectores x y = (fst x + fst y , snd x +snd y)

escalarVector :: Double -> Punto -> Punto

escalarVector x y = (fst y * x, snd y * x)

distancia :: Punto -> Punto -> Double

distancia x y = sqrt ((fst x - fst y )^2 + (snd x - snd y )^2)

-- Ej2

type Caja = (Double, Double,Double, Double)

primero:: Caja -> Double

primero (x,_,_,_) = x
segundo :: Caja -> Double
segundo (_, y, _, _) = y

tercero :: Caja -> Double
tercero (_, _, z, _) = z

cuarto :: Caja -> Double
cuarto (_, _, _, w) = w

solapan :: Caja -> Caja -> Bool


solapan x y = l1 && l2
    where 
        l1 = (primero x < primero y + tercero y) && (primero y < primero x + tercero x)
        l2 = (segundo x < segundo y + cuarto y) && (segundo y < segundo x + cuarto x)



--Ej3

ladoColision  :: Caja -> Caja -> String

ladoColision caja1 caja2 = lado
    where
    arriba =  abs(segundo caja2 - (segundo caja1 + cuarto caja1))
    abajo = abs(segundo caja1 - (segundo caja2 + cuarto caja2))
    izquierda = abs(primero caja1 - (primero caja2 + tercero caja2))
    derecha = abs(primero caja2 - (primero caja1 + tercero caja1))
    l = [arriba, abajo, izquierda, derecha]
    m = maximum l
    lista = zip  l ["arriba", "abajo", "izquierda", "derecha"] 
    lado = head [ nombre | (valor, nombre) <- lista, valor == m]

-- Ej4 

type Vector = (Double, Double)

splitOn :: Char -> String -> [String ]

splitOn _ [] = [""]
splitOn sep (x:xs)
    | x == sep  = "" : resto
    | otherwise = (x : head resto) : tail resto
  where
    resto = splitOn sep xs

trim :: String -> String
trim = trimFinal . trimPrincipio
  where
    trimPrincipio [] = []
    trimPrincipio (x:xs)
        | x == ' ' || x == '\t' || x == '\n' || x == '\r' = trimPrincipio xs
        | otherwise                                       = x:xs
    trimFinal = reverse . trimPrincipio . reverse

contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple condicion lista = sum [1 | x <- lista, condicion x]

list2Vector2 :: [Double] -> Vector

list2Vector2 [] = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [_] = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)

--Ej5

type Celda = Char
type Grid  = [String]

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


posicionesMeta :: Grid -> [(Int, Int)]
posicionesMeta grid =
    [ (i, j)
    | (i, fila) <- zip [0..] grid
    , (j, c)    <- zip [0..] fila
    , esMeta c
    ]


posicionesEnemigos :: Grid -> [(Int, Int, Char)]
posicionesEnemigos grid =
    [ (i, j, c)
    | (i, fila) <- zip [0..] grid
    , (j, c)    <- zip [0..] fila
    , esEnemigo c
    ]

agruparRachas :: String -> [(Int, Int)]
agruparRachas filaTexto = buscar 0 filaTexto
  where
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
