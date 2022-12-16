-- Actividad 1
data Edo = Q0 | Q1 | Q2 | Q3 | QF | QR Edo deriving Show -- Para poder indicar que se detuvo la ejecución, añadimos un estado final QF, que además, contiene una referencia al estado anterior a él.
type Q = [Edo]

-- Actividad 2
data Simbolo = A | B | X | Y | Blanco deriving Show

-- Actividad 3
-- Vamos a tratar como dos pilas a las dos listas. Hemos visto anteriormente que un autómata con dos pilas es equivalente a una máquina de turing. Usaremos éste modelo para representar nuestra máquina de Turing.
type Conf = ([Simbolo], Edo, [Simbolo])
-- Actividad 4
delta :: Conf -> Conf
-- delta (l, Q0, []) = Rechazar. Tal vez deberíamos usar una mónada para este punto.
delta (l, Q0, (A:xr)) = ((X:l), Q1, xr)
delta (l, Q0, (Y:xr)) = ((Y:l), Q3, xr)

delta (l, Q1, (A:xr)) = ((A:l), Q1, xr)
delta (l, Q1, (Y:xr)) = ((Y:l), Q1, xr)
delta ((l:ls), Q1, (B: xr)) = (ls, Q2, (l:Y:xr))

delta ((l:ls), Q2, (Y:xr)) = (ls, Q2, (l:Y:xr))
delta ((l:ls), Q2, (A:xr)) = (ls, Q2, (l:A:xr))
delta (l, Q2, (X:xr)) = ((X:l), Q0, xr)

delta (l, Q3, (Y:xr)) = ((Y:l), Q3, xr)
delta (l, Q3, []) = (l, QF, []) 

-- En el caso de que no esté definida la transición, mandamos a este estado de rechazo que anexamos. Su presencia facilita el reconocimiento de un análisis fallido.
delta (l, q, r) = (l, (QR q), r)

-- Actividad 5
delta_estrella :: Conf -> Conf
delta_estrella (l, QF, r) = (l, QF, r)
delta_estrella (l, (QR q), r) = (l, q, r)

delta_estrella conf = let next = delta conf in delta_estrella next

-- Actividad 6

-- Actividad 7

acepta :: Conf -> Bool
acepta conf = aux (delta_estrella conf)

aux :: Conf -> Bool 
aux (_, QF, _) = True
aux _ = False
-- Extra 1

-- Extra 2

-- Extra 3
