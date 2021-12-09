Program Formula
Implicit None
Real*8:: x = 3.2, y = 0.8, a = 0.81, y1, y11, yy1, y2, z11, cos, zz1, z1, z22, p
Open(6,file='result.txt')
Write (6,*) 'x =', x
y11 = x/(x**2+y**2)
yy1 = (y*(x-y)**2)/(x**4-y**4)
y1 = y11-yy1
Write(*,2) "y1 =",y1
2 FORMAT(A,F8.3)
y2 = 1/(x+y)
Write(*,1) "y2 =",y2
1 FORMAT(A,F8.3)
p = (2*Asin(1.))
Write (6,*) "p =", p
z11 = (cos((3./8)*p-(a/4))**2)
zz1 = (cos((11./8)*p+(a/4))**2)
z1 = z11-zz1
Write(*,3) "z1 =",z1
3 FORMAT(A,F8.3)
z22 = (sqrt(2.)/2) * sin(a/2)
Write(*,4) "y2 =",z22
4 FORMAT(A,F8.2)
print *, 'z1=',z1
print *, 'z1=',z22
End Program Formula