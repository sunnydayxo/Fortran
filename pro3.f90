Program Formula 
Implicit None
Integer::k
Real::  y, a, y1, y11, yy1, y2, z11, cos, zz1, z1, z22, p, x
Open(6,file='result.txt') 
Open(1,file='input.txt') 
do k=1,3
read (1,*)x,y,a
y11 = x/(x**2+y**2)
yy1 = (y*(x-y)**2)/(x**4-y**4)
y1 = y11-yy1
Write (6,*) "y1 =", y1
y2 = 1/(x+y)
Write (6,*) "y2 =", y2
p = (2*Asin(1.))
Write (6,*) "p =", p
z11 = (cos((3./8)*p-(a/4))**2)
zz1 = (cos((11./8)*p+(a/4))**2)
z1 = z11-zz1
Write (6,*) "z1 =", z1
z22 = (sqrt(2.)/2) * sin(a/2)
Write (6,*) "z2 =", z22
end do
End Program Formula