      SUBROUTINE SAMUS_XY_S_C
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display X-Y View of SAMUS SOUTH (+Z) C station
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
      DATA IFLAG,IFLAG2/1,7/
C----------------------------------------------------------------------
       CALL PSAMAG(IVIEW,IFLAG)
      CALL PSAMDETC(IVIEW,IFLAG2)
      CALL PSAMUD1_EV(IVIEW,IFLAG2)
      CALL PSAM_TK(IVIEW,IFLAG)
  999 RETURN
      END
