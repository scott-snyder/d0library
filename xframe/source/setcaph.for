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
      integer gzcaph,mycaph,gzproc,lproc
c
c     initialize
c
      lpoint = 0
c
      lproc = gzproc()
      if (lproc.lt.1) return
      mycaph = lq(lproc-4)
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
        else if (jcone.gt.4.5.and.jcone.lt.5.5) then
c
c         kt algorithm
c
          if (iq(mycaph+4).eq.5) then
            lpoint = lq(mycaph-2)
            return
          endif

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
