      SUBROUTINE CPTCAEC_ZR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Zero PTCAEC pointers.
C-
C-   Inputs  : none
C-   Outputs : zeroed arrays PTCAEC1, PTCAEC2
C-   Controls: If PTCZFL is true do nothing
C-
C-   Created  10-OCT-1989   Andrew P. White
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCAEC.INC'
      INTEGER NUMALL,NEZ,NL
C----------------------------------------------------------------------
      IF(PTCZFL) RETURN
C
      NEZ=MXETAMG-MNETAMG+1
      NL=MXLYMG-MNLYMG+1
      NUMALL=NEZ*NPHIL*NL
      CALL VZERO(PTCAEC1,NUMALL)
      CALL VZERO(PTCAEC2,NUMALL)
C
      PTCZFL=.TRUE.
C
  999 RETURN
      END
