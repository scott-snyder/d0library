      SUBROUTINE SELJET(w,tag,reason)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets jet type for physics display.  
C-
C-   Inputs  : w, reason ignore, tag=0,1,2 jet cone .3,.5,.7, 
C-                               tag=3 nearest neighbor
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-DEC-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      integer w,tag,reason
c
      include 'd0$xframe$source:d0map.inc'
c
      if (tag.eq.0) then
c
c       cone .3
c
        jcone = .3
      else if (tag.eq.1) then
c
c       cone .5
c
        jcone = .5
      else if (tag.eq.2) then
c
c       cone .7
c
        jcone = .7
      else if (tag.eq.3) then
c
c       nn
c
        jcone = 0.0
c
      else if (tag.eq.4) then
c
c       select pnut(1)
c
        pnuttype = 1
c
      else if (tag.eq.5) then
c
c       select pnut(2)
c
        pnuttype = 2
c
      else if (tag.eq.6) then
c
c       select pnut(3)
c
        pnuttype = 3
c
      endif
c
      return
      end
