--Euclidean Distance
sqrdist:: Float -> Float -> Float
sqrdist v w = (x*x)
	where
		x = v - w

sumd:: [Float] -> [Float] -> Float
sumd [] [] = 0.0
sumd (v:vs) (w:ws) = (sqrdist v w) + (sumd vs ws)

eucldist:: [Float] -> [Float] -> Float
eucldist v w = sqrt (sumd v w)

--Best Matching Unit
data Node = Nd {posX::Int, posy::Int, weight::[Float]}
			deriving (Show)

ed:: [Float] -> [Node] -> [(Node,Float)]
ed v [] = []
ed v (x:xs) =  (x,(eucldist v (weight x))) : ed v xs