      SUBROUTINE BKCSFH(LCSFH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CSFH
C-
C-   Inputs  : NONE
C-   Outputs : LCSFH: Link of Booked CSFH Bank
C-   Controls: None
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCSFH
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCSFH = 0
      IF(FIRST)THEN
        CALL MZFORM('CSFH','1I -F',IXIO)        ! Describe Bank format
        CALL STP_INZLNK
        FIRST = .FALSE.
      ENDIF
C
      CALL MZBOOK (IDVSTP,LCSFH,0,2,'CSFH',3,3,11,IXIO,0)
      CALL GZCSFH_INIT(LCSFH)
      IC(LCSFH+1) = 1  !version 1
  999 RETURN
      END
