      SUBROUTINE SAMUS_XY_TK_S
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display X-Y View of SAMUS SOUTH (+Z) tracks
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-MAY-1991 Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW,IFLAG
C______________________________________________________________________
      DATA IVIEW/2/
      DATA IFLAG/1/
C----------------------------------------------------------------------
      CALL PSAM_TK(IVIEW,IFLAG)
  999 RETURN
      END
