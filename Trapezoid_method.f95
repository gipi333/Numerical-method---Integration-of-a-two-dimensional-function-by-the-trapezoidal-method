!*******************************************************************************
!
! Année :2020/2021
!
! Auteur: Laboureur Guillaume
!
!*******************************************************************************


program sixth


implicit none
real::integre_2,a,b,t,c,d,m,n

real, parameter :: pi = 16*ATAN(0.2)-4*ATAN(1.0/239.0)



m = 100.0
n = 100.0
a = -5.0
b = 5.0
c = -5.0
d = 5.0

t = integre_2(a, b, c, d, n, m) 


end program  sixth





FUNCTION integre_2(a, b, c, d, n, m) 

	implicit none
	real,intent(in)::a,b,c,d,m,n

	real:: integre_2,delta_x,delta_y,un,deux,trois,quatre
	integer ::i,j,k
	real,dimension(99,99) :: matrice


	delta_x = ABS(b-a)/n
	delta_y = ABS(d-c)/m
	
	

	do i=1,99
		do j=1,99
		

			un = (f(a,c)+f(a,d)+f(b,c)+f(b,d))/4 * delta_x * delta_y
			deux = ( f(a+i*delta_x,c) + f(a+i*delta_x,d) )/2 * delta_x * delta_y
			trois = ( f(a,c+j*delta_y) + f(b,d+i*delta_y) )/2 * delta_x * delta_y
			quatre = f(a+i*delta_x,c+j*delta_y)

			matrice(i,j)  = un+deux+trois+quatre


			
		end do
	end do 
	
	open(unit=1,FILE='graphes.txt')
	write(1,*) matrice
	close(1)





	CONTAINS 		
		FUNCTION f(x,y)
			REAL :: f,y,x
            !real, parameter :: pi = 3.1415927
			real, parameter :: pi = 16*ATAN(0.2)-4*ATAN(1.0/239.0)
			

			f = y* COS(pi*x) + x*SIN(pi*y)
			return
		END FUNCTION 


END FUNCTION integre_2



