      SUBROUTINE BKFTRH(LKFTRH)
C-----------------------------------------------------------------------
C
C  Purpose and Methods : Books 'FTRH' header bank for FDC tracks 
C
C  Output: LKFTRH = Location of the booked bank in ZEBCOM.
C
C    Modified  ?-DEC-1988   Daria Zieminska 
C-   Updated   2-MAY-1989   Jeffrey Bantly  add in temporary link area 
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDLTRK 
C-   Updated  23-JAN-1991   Jeffrey Bantly  call FTRHFL to fill bank 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FDLTRK.INC/LIST'
      INCLUDE 'D0$LINKS:IZFTRH.LINK/LIST'
      INTEGER IXFTRH
      INTEGER LKZTRH,LKFTRH
      INTEGER GZZTRH,GZFTRH
C
      LOGICAL FIRST
C
      SAVE IXFTRH,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM('FTRH','2I 2F 6I',IXFTRH)
      ENDIF
C
      LKZTRH=GZZTRH()
      IF (LKZTRH.EQ.0) CALL BKZTRH(LKZTRH)
C
C ****  Bank FTRH, header for FDC Tracks and segments
C
      LKFTRH=GZFTRH()
      IF (LKFTRH.LE.0) THEN
        CALL MZBOOK(IXMAIN,LFTRH,LKZTRH,-IZFTRH,'FTRH',8,8,10,IXFTRH,0)
        LKFTRH = LFTRH
        CALL FTRHFL(LKFTRH)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
