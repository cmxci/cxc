import cmath

a = float(input("a = "))
b = float(input("b = "))
c = float(input("c = "))

d = (b**2) - (4*a*c)

solution1 = (-b-cmath.sqrt(d))/(2*a)
solution2 = (-b+cmath.sqrt(d))/(2*a)
print (solution1)
print (solution2)
