roots :: (Double, Double, Double) -> (Double, Double)
roots (a,b,c) = (((-b-d)/e), ((-b+d)/e))
    where d = sqrt(b^2 - 4*a*c)
          e = 2*a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a,b) = (a/d, b/d)
    where d = sqrt(a^2 + b^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a,b,c) = (a/d, b/b, c/d)
    where d=sqrt( a ^ 2 + b ^ 2 + c ^ 2 )

pole :: (Double, Double, Double) -> Double
pole (a,b,c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = 0.5*(a+b+c)