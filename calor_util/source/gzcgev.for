      FUNCTION GZCGEV()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to first CGEV bank
C-                         Must call GZCGEV_INIT with address of CGEV bank 
C-                         to initialize GZCGEV.
C-
C-   Returned value  : Link to 1st element of CGEV linear structure
C-   Inputs  : none
C-   Outputs : none
C-   Controls: 
C-
C-   Created 30-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC/LIST'
      INTEGER GZCGEV,NCGEV,GZCGEV_INIT,LCGEV
      LOGICAL FIRST
      SAVE NCGEV,FIRST
      DATA FIRST/.TRUE./, NCGEV/0/
C----------------------------------------------------------------------
C
      IF(NCGEV.GT.0) THEN
        GZCGEV = STP_LSLINK(NCGEV)
      ELSE
        GZCGEV = 0
C        CALL ERRMSG('NCGEV ZERO','GZCGEV','CALL GZCGEV_INIT','W')
      END IF
  999 RETURN
C---------------------------------------------------------------------
      ENTRY GZCGEV_INIT(LCGEV)
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL STP_GSLINK('GZCGEV',NCGEV )
      END IF
      IF(NCGEV.GT.0) THEN
        STP_LSLINK(NCGEV) = LCGEV
      ELSE
        CALL ERRMSG('NO_STP_LINKS_LEFT','GZCGEV','OOPS','W')
      END IF
      GZCGEV_INIT = NCGEV
 1999 RETURN
      END
