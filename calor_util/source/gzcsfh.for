      FUNCTION GZCSFH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CSFH bank
C-
C-   Returned value  : Link to 1st element of CSFH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC/LIST'
      INTEGER GZCSFH,NCSFH,GZCSFH_INIT,LCSFH
      LOGICAL FIRST
      SAVE NCSFH,FIRST
      DATA FIRST/.TRUE./, NCSFH/0/
C----------------------------------------------------------------------
C
      IF(NCSFH.GT.0) THEN
        GZCSFH = STP_LSLINK(NCSFH)
      ELSE
        GZCSFH = 0
C        CALL ERRMSG('NCSFH ZERO','GZCSFH','CALL GZCSFH_INIT','W')
      END IF
  999 RETURN
C---------------------------------------------------------------------
      ENTRY GZCSFH_INIT(LCSFH)
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL STP_GSLINK('GZCSFH',NCSFH )
      END IF
      IF(NCSFH.GT.0) THEN
        STP_LSLINK(NCSFH) = LCSFH
      ELSE
        CALL ERRMSG('NO_STP_LINKS_LEFT','GZCSFH','OOPS','W')
      END IF
 1999 RETURN
      END
