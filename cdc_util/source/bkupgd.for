      SUBROUTINE BKUPGD(LUPGD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book UPGD (upgrade header) bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUPGD.LINK'
      INTEGER LUPGD,LHITS,GZHITS
      INTEGER MPUPGD(5)
      DATA MPUPGD/4HUPGD,2,2,1,1/
      SAVE MPUPGD
C----------------------------------------------------------------------
      LHITS = GZHITS()
      IF (LHITS.LE.0) CALL BKHITS(LHITS)
      IF ( LHITS .LE. 0 ) THEN
        CALL ERRMSG('Unable to book HITS','BKUPGD',' ','W')
        LUPGD = 0
        RETURN
      ENDIF
C
C ****  Book UPGD bank
C
      CALL MZLIFT ( IXMAIN, LUPGD, LHITS, -IZUPGD, MPUPGD, 0 )
      IF ( LUPGD .LE. 0 ) THEN
        CALL ERRMSG('Unable to book UPGD','BKUPGD',' ','W')
        LUPGD = 0
        RETURN
      ENDIF
C
      IQ(LUPGD+1) = 1
  999 RETURN
      END
