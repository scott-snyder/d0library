      INTEGER FUNCTION LIBSAV()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save virtual display for later restore
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$COPY_VIRTUAL_DISPLAY
C----------------------------------------------------------------------
      LIBSAV=SMG$COPY_VIRTUAL_DISPLAY(MAINID,SAVEID)
      RETURN
      END
