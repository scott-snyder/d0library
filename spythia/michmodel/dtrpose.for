
	subroutine DTRPOSE (A,n)

c	This routine takes an nxn matrix A and finds its transpose, returning
c	the transpose as A
c	NOTE: This routine changes A, so A must be copied before TRPOSE is
c	called if it will be needed later. Of course, you could also just
c	call transpose again!
    
        implicit none
        integer i,j,n
    	real*8 A(n,n),p

	do i=1,n
	   p=0.
	   do j=i+1,n
		p=A(i,j)
		A(i,j)=A(j,i)
		A(j,i)=p
	   enddo
	enddo

	return
	end
