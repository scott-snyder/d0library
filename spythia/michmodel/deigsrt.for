
	subroutine DEIGSRT(D,V,n)

	implicit real*8 (a-h,o-z)
	dimension D(n),V(n,n)
	
	do i=1,n-1
	   k=i
	   p=D(i)
	   do j=i+1,n
		if (D(j).le.p) then
		   k=j
		   p=D(j)
		endif
	   enddo
	   if (k.ne.i) then
		D(k)=D(i)
		D(i)=p
		do j=1,n
		   p=V(j,i)
		   V(j,i)=V(j,k)
		   V(j,k)=p
		enddo
	   endif
	enddo

	return
	end
