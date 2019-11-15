-- wzorzec colection pipeline filter map fold (4 mozliwosci, 2 lewe , dwa prawe) zip zipwith concat

-- funckcje wizszego rzedu, koncepcja, napisac funckje ktora zwroci funckej jako wynik,
-- funkcja przyjmujaca funkcje jako parametr
-- umiec capisac np filter prawodlowo semantycznie
-- np napisz imoplementacje map dla listy

-- napisac funckcje aninimowa , np zapisz w postaci lambdy (*2) wiec \x -> x*2
--lub np napisz typ (\x -> x+1.5) 1

f1 = (\x->x-2)

f2 = (\x y -> sqrt (x^2 + y^2))

f3 = (\x y z -> sqrt (x^2 + y^2 + z^2))

x2 = (\x -> 2*x)

x2' = (\x -> x*2)

x2'' = (\x -> 2^x)

x3 = (\x -> x^2)

y2 = (\x -> 2/x)

y3 = (\x -> x/3)

y4 = (\x->4-x)

sqrt' = (\x -> sqrt x)

abs' = (\x-> if x < 0 then (-x) else x)

log' = (\x->log x)

id' = (\x -> id x)
 
const' = (\x y -> const x y)

f7 x = if x `mod` 2 == 0 then True else False

f7' = (\x -> if x`mod`2==0 then True else False)

f8 x = let y = sqrt x in 2 * y^3 * (y + 1)

f8' = (\x-> let y = sqrt x in 2 * y^3 * (y + 1) )

f9 1 = 3
f9 _ = 0

f9' = (\x -> if x == 1 then 3 else 0)