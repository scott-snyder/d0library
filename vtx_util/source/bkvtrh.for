      SUBROUTINE BKVTRH(LVTRH)
C-----------------------------------------------------------------------
C  Subroutine BKVTRH books 'VTRH' header bank for VTX tracks 
C
C  Output:
C    LVTRH       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska May 29 1987
C-   Updated  25-FEB-1994   Al Clark  Add 6th link to VCTH 
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'
      INTEGER  LZTRH,GZZTRH,LVTRH 
C
      LZTRH=GZZTRH()
      IF (LZTRH.EQ.0) CALL BKZTRH(LZTRH)
      LVTRH=LQ(LZTRH-IZVTRH)
      IF (LVTRH.LE.0) THEN
        CALL MZBOOK(IXMAIN,LVTRH,LZTRH,-IZVTRH,'VTRH',6,6,10,2,0)
      ENDIF
      RETURN
      END
