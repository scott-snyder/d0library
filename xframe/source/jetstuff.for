      SUBROUTINE JETSTUFF(nd,lpjet,vect)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  07-JUL-1994   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$inc:zebcom.inc'
c
      integer lpJET,nd
      real vect(*)
c
      integer iname,i,j
      character*4 cname
c
      equivalence (iname,cname)
c
      iname = iq(lpjet-4)
      call uzero(vect,1,41)
      nd = 0
      if (cname.ne.'JETS') return
      nd = iq(lpjet-1)
c
      vect(1) = iq(lpjet+1)               !version
      do i=2,min(14,nd)
        vect(i) = q(lpjet+i)
      enddo
      do i=15,16
        if (nd.lt.i) return
        vect(i) = iq(lpjet+i)
      enddo
      do i=17,19
        if (nd.lt.i) return
        vect(i) = q(lpjet+i)
      enddo
      do i=20,21
        if (nd.lt.i) return
        vect(i) = iq(lpjet+i)
      enddo
      do i=22,25
        if (nd.lt.i) return
        vect(i) = q(lpjet+i)
      enddo
      do i=26,27
        if (nd.lt.i) return
        vect(i) = iq(lpjet+i)
      enddo
      do i=28,35
        if (nd.lt.i) return
        vect(i) = iq(lpjet+i)
      enddo
c
      return  
      end
