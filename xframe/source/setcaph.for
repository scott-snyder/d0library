      SUBROUTINE SETCAPH(lpoint)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : looks in d0map.inc for jcone, sets lpoint to
C-                         point to the first jet bank for the algo
C-                         jcone = 0 use nearest neighbor
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-DEC-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer lpoint
c
      integer gzcaph,mycaph
c
c     initialize
c
      lpoint = 0
c
      mycaph = gzcaph()
      if (mycaph.lt.1) return
c
c     loop over caph banks, find the correct one
c
      do while (mycaph.gt.0)
        if (jcone.lt.0.01) then
c
c         nearest neighbor
c
          if (iq(mycaph+4).eq.3) then
            lpoint = lq(mycaph-2)
            return
          endif
c
        else
c
c         cone algo
c
          if (iq(mycaph+4).eq.2.and.abs(q(mycaph+6)-jcone).lt.0.01) then
            lpoint = lq(mycaph-2)
            return
          endif
        endif
c
c       next?
c
        mycaph = lq(mycaph)
      enddo
c
      return
      end
