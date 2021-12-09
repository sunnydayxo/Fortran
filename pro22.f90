Program Region !dots in figure
Implicit None
Real x,y 
Integer alpha
Integer, parameter :: inside=1, outside=2!
Logical A, B
Open(3,FILE="3.txt") 
Open(4,FILE="4.txt") 
Open(5,FILE="5.txt") 
Open(6,FILE="6.txt") 
Open(7,FILE="7.txt") 
Open(inside,FILE="in.txt") 
Open(outside,FILE="out.txt") 
Write(3,*)-3,4; Write(3,*)3,4 
Write(4,*)-3,2; Write(4,*)3,2 
Write(5,*)-3,4; Write(5,*)-3,2
Write(6,*)3,4; Write(6,*)3,2 
Do alpha=0,360,3 
 x=3*Cosd(real(alpha)); y=3*Sind(real(alpha))
 Write(7,*)x,y; 
Enddo 
Do x =-4,4,0.4 
 Do y =-4,5,0.4 
 A = y>=2.and.y<=4.and.abs(x)<3 
 B =(x*x+y*y)<=9.and.x<=0 
 If (A.or.B) then
 Write(inside, *) x, y 
 Else 
 Write(outside, *) x, y 
 End If 
 End Do 
End Do 
End Program Region