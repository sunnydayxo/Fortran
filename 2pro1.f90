Program Region!dots in figure
Implicit none
Real x,y
Integer alpha 
Integer, parameter:: inside = 1, outside = 2
Logical A, B, C
Open(3, FILE= '3.txt')
Open(4, FILE= '4.txt')
Open(5, FILE= '5.txt')
Open(inside, FILE= 'in.txt')
Open(outside, FILE= 'out.txt')
Write(3,*)-3,3 ; Write(3,*)0,0
Write(3,*)-3,3 ; Write(3,*)0,3
Write(4,*)0,0 ; Write(4,*)0,0
Write(4,*)-3,-3 ; Write(4,*)0,-3
Do alpha = 270, 450, 3
x = 3*cosd(Real(alpha)); y =3*sind(Real(alpha))
Write(7,*)x,y;
End Do 
Do x = -3, 3, 0.3
	Do y = -3, 3, 0.
		A = (y>-x) .and. y>=0 .and. y<=3 .and. x<=0 .and. x>=-3 
		B = x*x + y*y <= 9  .and. x>=0
		C = (-x<-y) .and. x>=-3 .and. y>=-3 .and. y<0 .and. x<=0 
		If (A .or. B .or. C) then 
			Write(inside, *) x, y 
		Else 
			Write(outside, *) x, y 
		End if 
	End Do 
End Do
End Program Region