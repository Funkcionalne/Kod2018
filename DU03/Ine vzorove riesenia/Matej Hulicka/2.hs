{-

[x | x<-xs] = xs
[f x | x <- xs] = map f xs
[e | x <- xs, p x,...] = [e | x <- filter p xs, ...]
[e | x <- xs, y <- ys, ...] = concat[[e | y <- ys, ...] | x <- xs]




a) 
[x|xs<-xss, x<-xs, odd x]
[x | xs<-xss, x <- filter odd xs]
concat [[x | x <- filter odd xs] | xs <- xss]
concat [filter odd xs | xs <- xss]
-- filter odd (concat xss)

quickCheck(\xss -> (filter odd (concat xss)) == [x|xs<-xss, x<-xs, odd x])





b) 
[(x,y)|x<-xs,p x, y<-ys]
[(x,y) | x <- filter p xs, y <- ys]
concat [[(x,y) | y <-ys] | x <- filter p xs]
concat [map (\y -> (x,y)) ys | x <- filter p xs]
-- concat (map (\x -> map (\y -> (x,y)) ys) (filter p xs))

quickCheck(\(xs, ys, p) -> (concat (map (\x -> map (\y -> (x,y)) ys) (filter p xs))) == [(x,y)|x<-xs,p x, y<-ys])

}-