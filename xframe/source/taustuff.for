      SUBROUTINE TAUSTUFF(lptau,vect)
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
      integer lptau
      real vect(*)
c
      integer iname,i,j
      character*4 cname
c
      equivalence (iname,cname)
c
      iname = iq(lptau-4)
      call uzero(vect,1,41)
      if (cname.ne.'PTAU') return
c
      vect(1) = iq(lptau+1)              !version
      vect(2) = q(lptau+3)               !ex
      vect(3) = q(lptau+4)               !ey
      vect(4) = q(lptau+5)               !ez
      vect(5) = q(lptau+6)               !e
      vect(6) = q(lptau+7)               !et
      vect(7) = q(lptau+8)               !theta
      vect(8) = q(lptau+9)               !phi
      vect(9) = q(lptau+10)             !eta
      vect(10) = q(lptau+11)             !r rms   
      vect(11) = q(lptau+12)             !et hottest
      vect(12) = q(lptau+13)             !et 2nd hottest
      vect(13) = iq(lptau+14)            !quality flag
      vect(14) = iq(lptau+15)            !ecor status
      vect(15) = q(lptau+16)             !E(1x1) cal
      vect(16) = q(lptau+17)             !E(3x3) cal
      vect(17) = q(lptau+18)             !E(5x5) cal
      vect(18) = iq(lptau+19)             !#CDC 10 deg cone
      vect(19) = iq(lptau+20)             !#CDC 20 deg cone
      vect(20) = iq(lptau+21)             !#CDC 30 deg cone
c
      return  
      end
