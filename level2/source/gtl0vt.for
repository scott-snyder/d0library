      SUBROUTINE GTL0VT(FASTZ,FAST_FLAG,SLOWZ,SLOW_FLAG,OK)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : THIS ROUTINE WILL EXTRACT THE LEVEL0
C-                         DATA FROM THE L0VT BANK
C-
C-   INPUTS  : NONE
C-   OUTPUTS : FASTZ Z-POS FROM FAST Z BOARD
C              SLOWZ Z-POS FROM SLOW Z BOARD
C              FAST_FLAG 
C                1= good according to fast z board
C                2= bad according to fast z board, or no L0 info at all
C              SLOW_FLAG MUTILPLE-INTERACTION Flag
C                 0= no slow vertex information
C                 1= almost certain of good single interaction
C                 2= reasonably certain (about 60%) of good single interaction
C                 3= reasonably certain (about 60%) of multiple interaction
C                 4= almost certain of multiple interaction
C              OK- TRUE if found ANY L0 information
C-   CONTROLS: NONE
C-
C-   CREATED   5-JUN-1992   TOM FAHLAND
C-   Updated  19-JUL-1992   James T. Linnemann comments; get all contents
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL OK,GOOD
      INTEGER GZL0VT, ZBIN, GOOD_EVT
      INTEGER LL0VT ,FAST_FLAG,SLOW_FLAG
      REAL SLOWZ, FASTZ
C----------------------------------------------------------------------
      FASTZ = 0
      SLOWZ = 0
      FAST_FLAG = 0
      SLOW_FLAG = 2

      OK =.FALSE.

      LL0VT = GZL0VT()
      IF (LL0VT.LE.0) CALL L0VTFL       !try to build the bank
      LL0VT = GZL0VT()
      OK = LL0VT.GT.0
      IF (OK) THEN
        FASTZ = Q(LL0VT+2)
        SLOWZ = Q(LL0VT+4)
        FAST_FLAG = IQ(LL0VT+3)
        SLOW_FLAG = IQ(LL0VT+5)      ! get slow z info
      ENDIF
C
  999 RETURN
      END
