program test
implicit none
integer, parameter :: N = 5

real    :: a(N,N),  b(N),mini(N), maxi(N)
real    :: summa
integer :: j, i
b = 0.0  
mini = 0.0
maxi = 0.0             
Open(1,file='in.txt')
Open(8,file='out.txt')
read(1,*)a          

do j = 1, n         
  summa = 0
do i = 1, N
    summa = summa + a(j,i)
end do
    b(j) = (summa-minval(a(j,:)) - maxval(a(j,:)))/(N-2)
    maxi(j)=maxval(a(j,:))
    mini(j)=minval(a(j,:))

end do                  
    write(8,*)maxi, ' - Максимумы столбцов' 
    write(8,*)mini, ' - Минимумы столбцов'

write(8,*)b,'среднее значение без минимумов и максимумов'
end program test