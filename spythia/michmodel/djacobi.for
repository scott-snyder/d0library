
	subroutine DJACOBI(A,n,D,V,nrot)

c	[From Numerical Recipes, p.309.]
c
c	Calculates the eigenvectors and eigenvalues of the nxn matrix A,
c	returning the nth eigenvalue in the array D(n) and the nth
c	eigenvector as the nth column of the nxn matrix V. 
c	The similarity transformationa that holds in this system is one
c	of V^(-1)AV = A(diagonal).
c
c	Note: This procedure varies from that in Num.Rec. in that it only
c	      passes the exact array size, and not also a larger value
c	      NP > N, for dimensioning the arrays.

c	Input:	n = dimension of matrix
c		A = nxn real symmetric matrix to diagonlize
c
c	Output: A loses its superdiagonal terms, but diagonal and subdiagonal
c		are intact.
c		D = vector containing eigenvalues of A
c		V = matrix whose columns are the eigenvectors of A. The nth
c		    column corresponds to the nth eigenvalue in D.
c		nrot = Number of Jacobi rotations performed.

	parameter(nmax=100)
	implicit real*8 (a-h,o-z)
	dimension A(n,n), D(n), V(n,n), B(nmax), Z(nmax)

	do ip=1,n
	   do iq=1,n
		v(ip,iq)=0.
	   enddo
	   V(ip,ip)=1.
	enddo

	do ip=1,n
	   B(ip)=A(ip,ip)
	   D(ip)=B(ip)
	   Z(ip)=0.
	enddo

	nrot=0
	do i=1,50
	   sm=0.
	   do ip=1,n-1
		do iq=ip+1,n
		   sm=sm+abs(A(ip,iq))
		enddo
	   enddo
	   
	   if(sm.eq.0.) return
	   if(i.lt.4) then
		tresh=0.2*sm/n**2
	   else
		tresh=0.
	   endif

	   do ip=1,n-1
		do iq=ip+1,n
		   g=100.*abs(A(ip,iq))
		   if ((i.gt.4).and.(abs(D(ip))+g.eq.abs(D(ip)))
     &		    .and.(abs(D(iq))+g.eq.abs(D(iq)))) then
			A(ip,iq)=0.
		   else if (abs(A(ip,iq)).gt.tresh) then
			h=D(iq)-D(ip)
			if (abs(h)+g.eq.abs(h)) then
			   t=A(ip,iq)/h
			else
			   theta=0.5*h/A(ip,iq)
			   t=1./(abs(theta)+sqrt(1.+theta**2))
			   if (theta.lt.0.) t=-t
			endif
			c=1./sqrt(1.+t**2)
			s=t*c
			tau=s/(1.+c)
			h=t*A(ip,iq)
			Z(ip)=Z(ip)-h
			Z(iq)=Z(iq)+h
			D(ip)=D(ip)-h
			D(iq)=D(iq)+h
			A(ip,iq)=0.
			do j=1,ip-1
			   g=A(j,ip)
			   h=A(j,iq)
			   A(j,ip)=g-s*(h+g*tau)
			   A(j,iq)=h+s*(g-h*tau)
			enddo
			do j=ip+1,iq-1
			   g=A(ip,j)
			   h=A(j,iq)
			   A(ip,j)=g-s*(h+g*tau)
			   A(j,iq)=h+s*(g-h*tau)
			enddo
			do j=iq+1,n
			   g=A(ip,j)
			   h=A(iq,j)
			   A(ip,j)=g-s*(h+g*tau)
			   A(iq,j)=h+s*(g-h*tau)
			enddo
			do j=1,n
			   g=V(j,ip)
			   h=V(j,iq)
			   V(j,ip)=g-s*(h+g*tau)
			   V(j,iq)=h+s*(g-h*tau)
			enddo
		   	nrot=nrot+1
		   endif
		enddo
	   enddo
	   do ip=1,n
		B(ip)=B(ip)+Z(ip)
		D(ip)=B(ip)
		Z(ip)=0.
	   enddo
	enddo
	pause '50 iterations should never happen'
	return
	end
