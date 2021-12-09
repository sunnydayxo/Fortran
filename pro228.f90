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
WRITE(*,"(4A)")  ( " No     An			 Sn  		  F		Delta    " )
WRITE(*,"(4A)")  ( "------------------------------------------------ " )
Sn=(x-1)/x
An=(x-1)/x
Do while (ABS(An) > eps)
An=(((x-1)*(N+1))/((N+2)*x))*An
Sn=Sn+An
F=log(x)
Delta=ABS(F-Sn)
WRITE(*,"(I4.2,E12.3,E12.3,F9.5,F9.5)")N, An, Sn, F, Delta
N=N+1
End Do
End Program Pro1