      SUBROUTINE BKVFIT(LVFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book routine for VFIT bank
C-
C-   Inputs  : none
C-   Outputs : LVFIT: bank address for VFIT bank
C-
C-   Created  15-MAY-1993   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'                             
      INCLUDE 'D0$LINKS:IZVFIT.LINK'                             
      INTEGER LVERH, GZVERH, LVFIT 
      INTEGER MPVFIT(5) 
      LOGICAL FIRST 
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA MPVFIT/0, 1, 0, 15, 0/
C------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('VFIT','1I 1B 13F',MPVFIT(5))
        CALL UCTOH('VFIT',MPVFIT(1),4,4)
      END IF
      LVERH = GZVERH()
      IF (LVERH .LE. 0) CALL BKVERH(LVERH)
      IF (LVERH .GT. 0) THEN
        LVFIT = LQ(LVERH - IZVFIT)
        IF (LVFIT .LE. 0) THEN
          CALL MZLIFT(IXMAIN,LVFIT,LVERH,-IZVFIT,MPVFIT,0)
          IQ(LVFIT - 5) = 1
        ELSE
          CALL MZLIFT(IXMAIN,LVFIT,LVFIT,0,MPVFIT,0)
        ENDIF
      ELSE
        LVFIT = 0
      ENDIF
      IQ(LVFIT+1) = 1        ! version #
C
  999 RETURN
      END
