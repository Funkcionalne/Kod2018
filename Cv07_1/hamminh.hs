    module Rozcvicka7 where
     
    kolko2 0=0
    kolko2 1=1
    kolko2 x=1+kolko2 (x `div` 2)
     
    kolko23 0=0
    kolko23 x=kolko2 x + kolko23 (x`div` 3)
     
    get_bounds 1 prev = (1,2)
    get_bounds n prev = if (kolko23 prev) < n && (kolko23 (prev*2) >= n) then (prev,prev*2 + 1) else get_bounds n (prev * 2)
     
    bin_search n (low,high)
    	| (kolko23 pivot == n) && (kolko23 (pivot - 1) == (n-1)) = pivot
    	| (kolko23 pivot >= n) && (kolko23 (pivot-1) >= n) = (bin_search n (low, pivot))
    	| otherwise = (bin_search n (pivot, high))
    	where pivot = div (low + high) 2
     
    nty n = bin_search (n+1) (get_bounds (n+1) 1)
    