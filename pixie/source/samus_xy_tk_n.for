      SUBROUTINE SAMUS_XY_TK_N
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display X-Y View of SAMUS NORTH (-Z) tracks
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
      INTEGER IVIEW,IFLAG,IFLAG2
C______________________________________________________________________
      DATA IVIEW/2/
      DATA IFLAG,IFLAG2/-1,2/
C----------------------------------------------------------------------
      CALL PSAM_TK(IVIEW,IFLAG)
  999 RETURN
      END
