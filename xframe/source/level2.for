      SUBROUTINE LEVEL2(n2,bits,name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns a list of level 1 triggers passing
C-
C-   Inputs  : 
C-   Outputs : N1  - number of triggers passing
C-             NAME - name of passing triggers
C-             BITS - bits of passing triggers
C-   Controls: 
C-
C-   Created  14-SEP-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$inc:zebcom.inc'
c
      integer n1,n2,bits(*)
      character*(*) name(*)
c
      integer pointer,iline,i,i1,j
      character*4 cline
c
      equivalence (iline,cline)
c
      n1 = 0
c
c     first get first ESUM bank
c
      call blocat(ixmain,'TSUM',pointer)
      if (pointer.le.0) return
c
      n1 = iq(pointer+4)
      n2 = iq(pointer+5)
      if (n1.lt.1) return
c
c     fetch level 2 triggers
c
      i1 = 6 + (n1*iq(pointer+3))
      do i=1,n2
        bits(i) = iq(pointer+i1)
        do j=1,8
          iline = iq(pointer+i1+j)
          name(i)(4*j-3:4*j) = cline
        enddo
        i1 = i1 + iq(pointer+3)
      enddo
c
      return
      end
