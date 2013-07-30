--Exponential Decay

expdecay:: Float -> Int -> Int -> Float
expdecay s n i = s * (exp (- (intToFloat i) / ((intToFloat n) / log s)))

--Euclidean Distance
	
sumd:: [Float] -> [Float] -> Float
sumd [] [] = 0.0
sumd (v:vs) (w:ws) = (v - w) ^ 2 + (sumd vs ws)

eucldist:: [Float] -> [Float] -> Float
eucldist v w = sqrt (sumd v w)

--vectorvectorminus

vectorminus:: [Float] -> [Float] -> [Float]
vectorminus [] [] = []
vectorminus (x:xs) (y:ys) = [x - y] ++ vectorminus xs ys

--vectorsum

vectorsum:: [Float] -> [Float] -> [Float]
vectorsum [] [] = []
vectorsum (x:xs) (y:ys) = [x + y] ++ vectorsum xs ys

--Nodes

data Node = Nd {posX::Int, posY::Int, weight::[Float]}
			deriving (Show)
			
--Distance from node to edge of a square grid in number of nodes
			
distoedge:: Int -> Node -> Int
distoedge n p = minimum ([n - (posX p)] ++ [n - (posY p)] ++ [posX p] ++ [posY p])

--Distance between Nodes

intToFloat :: Int -> Float
intToFloat n = fromInteger.toInteger n

nodesdist:: Node -> Node -> Float
nodesdist p q = eucldist ([intToFloat(posX p)] ++ [intToFloat(posY p)]) ([intToFloat(posX q)] ++ [intToFloat(posY q)])

--Best Matching Unit/s

ediv:: [Float] -> [Node] -> [Float]
ediv v [] = []
ediv v (p:ps) =  eucldist v (weight p) : ediv v ps

bmu:: [Node] -> [Float] -> [Node]
bmu p v = [p' | p' <- p,  eucldist v (weight p') == minimum (ediv v p)]

--Nodes weight adjustment fuction (by iteration, total iterations, bmu, node and input vector)

distfactor:: Int -> Int -> Int -> Node -> Node -> Float
distfactor i n g b p = exp (-1.0 * (eucldist (weight b) (weight p))^2 / (2.0 * (expdecay (distoedge g b) n i)))

neweight:: Int -> Int -> Int -> Node -> Node -> [Float] -> [Float]
neweight i n g b p v = vectorsum (weight p) (map (((distfactor i n b p) * (expdecay (distoedge g b) n i)) *) (vectorminus v (weight p)))

-- SOM for bitmap
--somap:: [(Int,Int,Int)] -> [(Int,Int,Int)]
--somap (x:xs) = 
