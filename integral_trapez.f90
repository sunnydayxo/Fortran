program trapezoid
implicit none
real, external :: f 
real :: a, b, h, p, integration, summ
integer:: n, i 
print*, "lower"
read*,a
print*, "higher"
read*,b 
print*, "n"
read*,n 
h = (b-a)/n 
p=(h/2.)*(f(a)+f(b))
summ = 0
do i=1, n-1
summ = summ+h*f(a+i*h)
end do 
integration = p+summ
print*, "result", integration 
print*, summ
end

real function f(x)
f = sqrt(1+x**2)
end function