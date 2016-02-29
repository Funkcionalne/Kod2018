module RozdielParnychNeparnych where

rozdielParnychNeparnych	:: [Integer] -> Integer
rozdielParnychNeparnych	xs = abs(sum ps - sum ns)
		where (ps, ns) = foldr	(\x -> \(p,n)->(n,x:p)) ([],[])	xs	
