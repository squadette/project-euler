
primes :: [Integer]
primes = 2: 3: sieve 0 (tail primes) 5

sieve k (p:ps) x = [x | x<-[x,x+2..p*p-2], and [x`rem`p/=0 | p<-fs]]
                   ++ sieve (k+1) ps (p*p+2) 
    where fs = take k (tail primes)

result007 n = primes !! (n-1)

main :: IO ()
main = print $ result007 10001
