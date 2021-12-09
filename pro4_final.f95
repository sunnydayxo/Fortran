Program Vector !array
Implicit None
Real,dimension(1:12):: M
Real:: H,G,s,R
Integer:: N,E,k,q, j=0
Open(1,FILE='In.txt') 
Open(8,FILE ='Out.txt') 
Read(1,*)M 
write(8,*)'Исходный массив: '
write(8,2) M 
2 format (F8.2)
read (*,*)N
do k=1,N 
    if (M(k)>0) then
    H=H+M(k)
    end if
end do
write(8,'(A,F8.2,A)')'H = ',H, ' - сумма положительных среди N первых элементов'

do k=1,12 
if (M(k)<0) then 
    if (j == 0) then 
    E = k 
    j = 1
    end if
    s = s + M(k) 
    q = q + 1 
    end if
end do

G = s/q 
R = (H / ((E+1)*(H+1)))+G 
write(8,'(A,I8,A)')'E = ',E, ' - номер первого отрицательного элемента'
write(8,'(A,F8.2,A)')'G = ',G, ' - среднее арифметическое отрицательных элементов' 
write(8,'(A,F8.2,A)')'R = ',R, ' - результат выражения данного в варианте'
End program Vector
