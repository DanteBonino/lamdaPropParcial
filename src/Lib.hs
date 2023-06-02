module Lib () where
--Cosas que da el enunciado:
type Requisito = Depto -> Bool
type Busqueda = [Requisito]
type Depto = (Int, Int, Int, String)
type Persona = (String, [Busqueda])

ambientes (a, _,_,_) =a
superficie (_,m2,_,_) = m2
precio (_,_,p,_)= p
barrio (_,_,_,b) = b

mail persona = fst persona
busquedas persona = snd persona

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = x <= z && y >= z

deptosDeEjemplo = [(3,80,7500,"Palermo"), (1,45,3500,"Villa Urquiza"), (2,50,5000,"Palermo"), (1,45,5500,"Recoleta")]

--Punto 1:
mayor :: (Ord b) => (a -> b) -> a -> a -> Bool
mayor  = cumpleCondicionSegun (flip (<)) 

menor :: (Ord b) => (a -> b) -> a -> a -> Bool
menor  = cumpleCondicionSegun (flip (>))

cumpleCondicionSegun :: (Ord b) => (b -> b -> Bool) -> (a -> b) -> a -> a -> Bool
cumpleCondicionSegun unaCondicion unaFuncion unValor = (unaCondicion (unaFuncion unValor) . unaFuncion)

--b
ordenarSegunLongitud :: ((String -> Int)->String-> String -> Bool)-> [String] -> [String]
ordenarSegunLongitud funcion = ordenarSegun (funcion length)

--Si queres ordernar por mayor:
-- ordernarSegunLongitud mayor ["a", "aa", "aaa"] y te queda -> ["aaa", "aa", "a"]
--Si queres ordenarPorMenor:
-- ordernarSegunLongitud menor ["a", "aa", "aaa"] y te queda -> ["a", "aa", "aaa"]

--Punto 2:
--a
ubicadoEn :: [String] -> Depto -> Bool
ubicadoEn unosBarrios unDepartamento = elem (barrio unDepartamento) unosBarrios

--b
cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango unaFuncion cotaInferior cotaSuperior = (between cotaInferior cotaSuperior . unaFuncion) 

departamentoDeEjemplo :: Depto
departamentoDeEjemplo = (1,2,3,"Palermo")

--Punto 3:
--a
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda unDpto = all ($ unDpto)

--b
buscar :: Busqueda -> (Depto -> Depto-> Bool) -> [Depto] -> [Depto]
buscar unosRequisitos unCriterio = (ordenarSegun unCriterio . filter (flip cumpleBusqueda unosRequisitos))

--c
{-
En la consola:
buscar [(ubicadoEn ["Recoleta", "Palermo"]), cumpleRango ambientes 1 2, seAlquilaAMenosDe 6000]  mayorSuperficie
-}

ejemploDeBuscar :: [Depto] -> [Depto]
ejemploDeBuscar = buscar [(ubicadoEn ["Recoleta", "Palermo"]), cumpleRango ambientes 1 2, seAlquilaAMenosDe 6000]  mayorSuperficie

seAlquilaAMenosDe :: Int -> Depto -> Bool
seAlquilaAMenosDe unPrecio = ((<unPrecio) . precio) -- También se podría hacer con cumpleRango precio 0 unPrecio

mayorSuperficie :: (Depto -> Depto -> Bool)
mayorSuperficie = (mayor superficie)




