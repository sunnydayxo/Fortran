module integration_methods
implicit none
contains
  real function trapezoid_rule(x, n, nn)
    real :: f
    real ::  a, b, nn
  integer, intent(in) :: n
  real, dimension(n), intent(inout) :: x
    integer :: k
    real :: s
    s = 0
    do k=1, n-1
      s = s + ((x(k)+x(k+1))/2*nn)
    end do  
    trapezoid_rule = s
  end function trapezoid_rule
end module integration_methods

program integration
  use integration_methods
  implicit none
  real, external :: s
  integer :: j=1, i=1, m = 400, n = 40
  Real, Allocatable, Dimension(:) :: Ya
  Real, Allocatable, Dimension(:) :: Yb
  real :: x=-3, integral, xx=-2
  ALLOCATE(Ya(1:n),Yb(1:m))
do while (x<=2.1)
     Ya(i) = exp(-x/5)
     i=i+1
     x=x+0.1
end do
  do while (xx<=2)
     Yb(j) = exp(-xx/5)
     j=j+1
     xx=xx+0.01
end do
  integral=trapezoid_rule(Ya,  40, 0.1)
  write (*,*) 'Trapezoid rule = ', integral
  integral=trapezoid_rule(Yb, 400,0.01)
  write (*,*) 'Trapezoid rule = ', integral
deallocate(Ya,Yb)
end program integration 