      SUBROUTINE PNUTSTUFF(lpnut,vect)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-FEB-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$inc:zebcom.inc'
c
      integer lpnut
      real vect(*)
c
      integer iname,i,j
      character*4 cname
c
      equivalence (iname,cname)
c
      iname = iq(lpnut-4)
      call uzero(vect,1,41)
      if (cname.ne.'PNUT') return
c
      vect(1) = iq(lpnut+1)              !version
      vect(2) = iq(lpnut+2)              !charge
      vect(3) = q(lpnut+3)               !ex
      vect(4) = q(lpnut+4)               !ey
      vect(5) = q(lpnut+5)               !ez
      vect(6) = q(lpnut+6)               !e
      vect(7) = q(lpnut+7)               !et
      vect(8) = q(lpnut+8)               !theta
      vect(9) = q(lpnut+9)               !eta
      vect(10) = q(lpnut+10)             !phi
      vect(11) = q(lpnut+11)             !sigex**2
      vect(12) = q(lpnut+12)             !sigey**2
      vect(13) = q(lpnut+13)             !siget 
      vect(14) = q(lpnut+14)             !etscalar
c
      return  
      end
