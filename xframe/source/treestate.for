      SUBROUTINE TREESTATE(what,state)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : toggles tree widget appearance
C-
C-   Inputs  : what (0=fetch, 1=set)  state (0=disable, 1=enable)
C-   Outputs :
C-   Controls:
C-
C-   Created   1-SEP-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer what,state
c
      if (what.eq.0) then
c
c       fetch state
c
        state = tstate
c
      else if (what.eq.1) then
c
c       set state
c
        tstate = state
c
      endif
c
      return
      end
