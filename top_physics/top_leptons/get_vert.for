      SUBROUTINE GET_VERT(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C
      INTEGER IER,RUNNO,EVONUM,XING
      INTEGER LVERH,LVERT,GZVERH
C----------------------------------------------------------------------
      RUN(1) = RUNNO(0)
      RUN(2) = EVONUM()
      RUN(3) = IQ(LHEAD+11)
      RUN(4) = IQ(LHEAD+15)
      RUN(5) = IQ(LHEAD+16)
      RUN(6) = IQ(LHEAD+17)
      RUN(7) = IQ(LHEAD+18)
      RUN(8) = XING
      RUN(9) = IQ(LHEAD+30)
      LVERH = GZVERH()
      IF(LVERH.GT.0)THEN
        NVERT = IQ(LVERH+2)
        LVERT = LQ(LVERH-IZVERT)
        IF(LVERT.GT.0)THEN
          ZVERT = Q(LVERT+5)
        ENDIF
      ENDIF
  999 RETURN
      END
