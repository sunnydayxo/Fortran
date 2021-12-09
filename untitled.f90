program test !matrix
implicit none
integer, parameter :: N = 6, M = 8

real    :: a(N,M),  b(N),mini(N), maxi(N)
real    :: summa
integer :: j, i
b = 0.0  
mini = 0.0
maxi = 0.0             
Open(1,file='in.txt')
Open(8,file='out.txt')
read(1,*)a          

do j = 1, N       
  summa = 0
do i = 1, M
    summa = summa + a(j,i)
end do
   maxi(j)=maxval(a(j,:))
    mini(j)=minval(a(j,:))
    b(j) = (summa-minval(a(j,:)) - maxval(a(j,:)))/(M-2)
end do                  
    write(8,*)maxi, ' - Максимумы столбцов' 
    write(8,*)mini, ' - Минимумы столбцов'
write(8,*)a
write(8,*)b,'среднее значение без минимумов и максимумов'
end program test