roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
    let d = sqrt (b*b - 4 *a*c)
        e = 2 *a
    in ((-b-d)/e, (-b+d)/e)


unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x,y) =
    let a = sqrt (x^2+y^2)
    in (x/a, y/a)
    

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x,y,z) =
    let a = sqrt (x^2+y^2+z^2)
    in (x/a,y/a,z/a)
    

tringleAreaHeron :: (Double, Double, Double) -> Double
tringleAreaHeron (a,b,c) = 
    let p = (0.5 * (a+b+c))
    in sqrt ( p*(p-a)*(p-b)*(p-c))
    