      SUBROUTINE MUOSTUFF(lpmuo,vect)
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
      integer lpmuo
      real vect(*)
c
      integer iname,i,j
      character*4 cname
c
      equivalence (iname,cname)
c
      iname = iq(lpmuo-4)
      call uzero(vect,1,41)
      if (cname.ne.'PMUO') return
c
      vect(1) = iq(lpmuo+1)               !version
      vect(2) = iq(lpmuo+2)               !charge
      if (vect(2).eq.14.) vect(2) = 1.
      if (vect(2).eq.-14.) vect(2) = -1.
      vect(3) = iq(lpmuo+3)               !flag for dedx
      vect(4) = iq(lpmuo+4)               !method of calc
      vect(5) = iq(lpmuo+5)               !track flag
      vect(6) = iq(lpmuo+6)               !number of cd tracks in cone
      vect(7) = iq(lpmuo+7)               !quadrant
      vect(8) = iq(lpmuo+8)               !method of fit
      vect(9) = iq(lpmuo+9)               !quality
      vect(10) =  q(lpmuo+10)             !px
      vect(11) =  q(lpmuo+11)             !py
      vect(12) =  q(lpmuo+12)             !pz
      vect(13) =  q(lpmuo+13)             !p 
      vect(14) =  q(lpmuo+14)             !pt
      vect(15) =  q(lpmuo+15)             !theta
      vect(16) =  q(lpmuo+16)             !eta
      vect(17) =  q(lpmuo+17)             !phi
      vect(18) =  q(lpmuo+21)             !sig p**2
      vect(19) =  q(lpmuo+22)             !sig pt**2
      vect(20) =  q(lpmuo+23)             !chi**2/dof
      vect(21) =  q(lpmuo+24)             !t0 offset
      vect(22) =  q(lpmuo+25)             !x coord track
      vect(23) =  q(lpmuo+26)             !y coord track
      vect(24) =  q(lpmuo+27)             !z coord track
      vect(25) =  q(lpmuo+33)             !e loss expected
      vect(26) =  q(lpmuo+34)             !e loss ob (cell+2)
      vect(27) =  q(lpmuo+35)             !e loss ob (cone .4)
      vect(28) =  q(lpmuo+36)             !e loss ob (cone .6)
      vect(29) =  q(lpmuo+37)             !mu/cd angle
      vect(30) =  q(lpmuo+38)             !d_phi    
      vect(31) =  q(lpmuo+39)             !d_theta  
      vect(32) =  q(lpmuo+40)             !cd cone
      vect(33) =  q(lpmuo+41)             !impact vtx
      vect(34) =  q(lpmuo+42)             !impact fit
      vect(35) =  q(lpmuo+43)             !e loss mu
      vect(36) = iq(lpmuo+46)             !hits on track a,b,c
      vect(37) = iq(lpmuo+47)             !fit "    
      vect(38) = iq(lpmuo+48)             !trigger 1
      vect(39) = iq(lpmuo+49)             !trigger 2
      vect(40) = iq(lpmuo+50)             !trigger 3 
      vect(41) = iq(lpmuo+51)             !trigger 4
      vect(42) = iq(lpmuo+55)             !vtx number
c
      return  
      end
