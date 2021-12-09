Program Pro338 !dots in figure
Implicit None
Real:: x = 0.6
Real:: eps = 1E-04
Integer:: N = 0 
Integer:: k
Integer:: Nmax = 100
Real:: An = 0.00, Sn = 0, F
Real Delta
Namelist /result/ x, F, Sn, n, Delta
Namelist /avaria/ x, An, Sn, F, N, Nmax
Open(1,file='input.txt') 
WRITE(*,"(4A)")  ( " No  	X   		An			 Sn  		  F		Delta    " )
WRITE(*,"(4A)")  ( "------------------------------------------------ " )
Sn=(x-1)/x
An=(x-1)/x
do while (x<1)
Do while (ABS(An) > eps)
An = (((x-1)*(N+1))/((N+2)*x))*An
Sn = Sn+An
F = log(x)
Delta=ABS(F-Sn)
WRITE(*,"(I4.2,E12.1,E12.3,E12.3,F9.5,F9.5)")N, X, An, Sn, F, Delta
N = N +1
End Do
N = 1
x = x+0.1
An = (((x-1)*(N+1))/((N+2)*x))*An
Sn = Sn+An
F = log(x)
Delta=ABS(F-Sn)
WRITE(*,"(I4.2,E12.1,E12.3,E12.3,F9.5,F9.5)")N, X, An, Sn, F, Delta
End do
End Program Pro338