      INTEGER FUNCTION LIBSNA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a snapshot of the screen
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$SNAPSHOT
C----------------------------------------------------------------------
      LIBSNA=SMG$SNAPSHOT(PASTID,1)
C&ELSE
C&      LIBSNA=1
C&ENDIF
      RETURN
      END
