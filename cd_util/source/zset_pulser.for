      SUBROUTINE ZSET_PULSER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the pulser to the specified TARGET and
C-                         pulser pattern
C-
C-   Inputs  : none,  inputs from common block
C-   Outputs : none
C-   Controls: none
C-
C-   Created   1-NOV-1990   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZPULSER.INC'
      INTEGER CALL_STATUS,PATTERN,ILEN
      CHARACTER*80 STRING
      CHARACTER*16 ISTRING
C
C----------------------------------------------------------------------
C
      CALL STR$TRIM(TARGET,TARGET,ILEN)
      CALL ZDEC_PULSER(PATTERN,AMPLTDA,AMPLTDB,POLARITY,QUADRANT,SHHALF,
     &  PREAMP,SHCARD,2)
      CALL MVBITS(CRTSEL,0,2,PATTERN,13)
C
      WRITE(ISTRING,100)PATTERN
  100 FORMAT(Z16.16)
      WRITE(STRING,110)TARGET,ISTRING(9:12),ISTRING(13:16),ISTRING(1:4),
     &  ISTRING(5:8)
  110 FORMAT(A12,4X,4(A4,3X))
      CALL STR$TRIM(STRING,STRING,ILEN)
C
      CALL SET_ITEM(STRING,CALL_STATUS)
      IF (CALL_STATUS.NE.0) THEN
        CALL INTMSG(' Time out, Pulser may not be set ')
      ENDIF
C
  999 RETURN
      END
