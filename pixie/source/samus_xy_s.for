      SUBROUTINE SAMUS_XY_S
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display X-Y View of SAMUS SOUTH (+Z)
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
       CALL PSAMAG(IVIEW,IFLAG)
      CALL PSAMDETC(IVIEW,IFLAG)
      CALL PSAMUD1_EV(IVIEW,IFLAG)
      CALL PSAM_TK(IVIEW,IFLAG)
  999 RETURN
      END
