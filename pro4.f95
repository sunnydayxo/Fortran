Program Vector
Implicit None
Real,dimension(1:12):: M
Real:: H,G,S,R,Y,P=0
Integer:: N=10,k,q, j=0
Open(1,FILE='In.txt') ! файл с исходным массивом
Open(8,FILE ='Out.txt') ! результирующий файл
Read(1,*)M ! ввод массива
write(8,*)'Исходный массив: '
write(8,2)M ! вывод массива 
write(8,'(A,I0)')'Вы ввели N=', N
2 format (F8.2)
do k=N,12 ! среднее геометрическое положительных среди N последних эелементов
    if (M(k)>0) then
    P=p+M(k)
    s=p**(1./k)
    end if
end do
write(8,'(A,F8.2,A)')'S = ',S, ' - среднее геометрическое '

do k=1,12 !номер первого отрицательного элемента
if (M(k)==0) then
    Q = k 
    end if
end do
R = sum(M)
Y = R/(Q+1)+S!конечное выражение
write(8,'(A,I8,A)')'Q = ',Q, ' - номер первого нулевого элемента'
write(8,'(A,F8.2,A)')'R = ',R, ' - сумма всех элементов' 
write(8,'(A,F8.2,A)')'Y = ',Y, ' - результат выражения данного в варианте'
End program Vector