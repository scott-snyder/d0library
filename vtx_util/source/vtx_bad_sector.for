      LOGICAL FUNCTION VTX_BAD_SECTOR(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : As of Feb 1994, only one sector is bad -- due to HV
C-               problem early in run 1A.
C-
C-   Returned value  : TRUE -- Don't use sector
C-   Inputs  : LAYER,SECTOR -- usual, starting from 0
C-   Outputs : 
C-   Controls: 
C-
C-   Created   20-Feb-1994   Ed Oltman
C-   Updated   2-SEP-1994   Ed Oltman  Add Layer 2 Sector 23 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYER,SECTOR,RUNNo
C----------------------------------------------------------------------
      VTX_BAD_SECTOR = .FALSE.
      IF (LAYER .EQ. 2) THEN
        IF (SECTOR .EQ. 0) THEN
          VTX_BAD_SECTOR = RUNNO() .GT. 56639
        ELSEIF(SECTOR .EQ. 23) THEN
          VTX_BAD_SECTOR = RUNNO() .GT. 80443
        ENDIF
      ENDIF
  999 RETURN
      END
