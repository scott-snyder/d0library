      SUBROUTINE PFWAIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Wait for a PF-key to be struck in full screen 
C-                         mode
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: PF is changed in this routine but acted upon externally 
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C----------------------------------------------------------------------
      PF=0                        !RESET to avoid looping over commands
      WAIFLG=.TRUE.               ! Indicate that PF came from this routine
      RETURN
      END
