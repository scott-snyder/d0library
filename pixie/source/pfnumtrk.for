      SUBROUTINE PFNUMTRK(NUMTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the number of FDC tracks by Half
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-JAN-1991   Jeffrey Bantly
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves 
C-   Updated   8-AUG-1991   Robert E. Avery  Pass NUMTRK back as an argument. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
      INTEGER LKFTRH,GZFTRH
      INTEGER LKFDCT
      INTEGER HALF,NHIT,NUMTRK(0:1)
      CHARACTER*50 TEXT
C----------------------------------------------------------------------
      NUMTRK(0)=0
      NUMTRK(1)=0
      LKFTRH=GZFTRH()
      IF(LKFTRH.LE.5) THEN
        CALL INTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
      LKFDCT=LQ(LKFTRH-IZFDCT)
      IF(LKFDCT.LE.5) THEN
        CALL INTMSG(' No FDC tracks present')
        GOTO 999
      ENDIF
C
  100 CONTINUE
      IF(LKFDCT.LE.5) GOTO 200
      NHIT=IQ(LKFDCT+2)
      IF(NHIT.LE.0) GOTO 100
      HALF=IAND(1,IQ(LKFDCT+1))
      IF(HALF.EQ.0 .OR. HALF.EQ.1) NUMTRK(HALF)=NUMTRK(HALF)+1
      LKFDCT=LQ(LKFDCT)
      GOTO 100
C
  200 CONTINUE
      CALL OUTMSG('1')
      WRITE(TEXT,201) NUMTRK(0),NUMTRK(1)
  201 FORMAT(' FDC Tracks in North FDC =',I4,',  South FDC =',I4)
      CALL INTMSG(TEXT)
C
C----------------------------------------------------------------------
  999 RETURN
      END
