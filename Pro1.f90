Program Pro1 !dots in figure
Implicit None
Real:: x = 0.6
Real:: eps = 1E-04
Integer:: N = 0
Integer:: Nmax = 100
Real:: An = 0, Sn = 0, F
Real Delta
Namelist /result/ x, F, Sn, n, Delta
Namelist /avaria/ x, An, Sn, F, N, Nmax
Open(1, file = 'Out.txt')!Result
Open(2, file = 'An.txt')!An ot N
Open(3, file = 'Sn.txt')!Sn ot N
Open(4, file = 'F.txt')! F
Write(1, *) 'Точность вычисления',eps
Sn=(x-1)/x
An=(x-1)/x
Do while (ABS(An) > eps)
An=(((x-1)*(N+1))/((N+2)*x))*An
Sn=Sn+An
Write(2, *)N ,An 
Write(3, *)N,Sn 
N=N+1
F=log(x)
Delta=ABS(F-Sn)
Write(1, *) Delta
Write(4, *) N, F 
End Do
End Program Pro1