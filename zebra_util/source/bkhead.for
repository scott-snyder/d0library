      SUBROUTINE BKHEAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank HEAD
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: None
C-
C-   Created   2-JUL-1990 15:40:44.15  Rajendran Raja
C-   Updated  25-JUL-1990   Serban D. Protopopescu   
C-   Updated  12-MAR-1992   Serban Protopopescu  (fix MZFORM) 
C-   Updated   5-MAY-1994   Serban Protopopescu  increase to 32 words 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOHEAD
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF(FIRST)THEN
C
        FIRST=.FALSE.
        CALL MZFORM('HEAD','1I 2H -I',IOHEAD)
        LHEAD=0
C
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LHEAD,LHEAD,1,'HEAD',18,18,32,IOHEAD,0)
      IQ(LHEAD+14)=6
C
  999 RETURN
      END
