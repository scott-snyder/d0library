      FUNCTION GZFDCT(ITRACK)
C----------------------------------------------------------
C
C  Returns pointer to Zebra bank FDCT for FDC track ITRACK
C
C-   Created  xx-DEC-1988   Daria Zieminska
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up
C-   Updated   5-JUN-1992   Susan K. Blessing  Only do LZFIND
C-    when both LKFTRH and LKFDCT are both zero.
C-   Updated  10-SEP-1992   sandor Feher  fixed for ITRACK=0
C-   Updated  11-FEB-1993   Robert E. Avery  fix the fix  for ITRACK=0
C
C----------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFDCT.LINK/LIST'
      INTEGER GZFDCT
      INTEGER ITRACK,LKFDCT,LKFTRH
      INTEGER GZFTRH,LZFIND
C--------------------------------------------------------------
      GZFDCT=0
      LKFTRH=GZFTRH()
      IF(LKFTRH.NE.0) THEN
        LKFDCT=LQ(LKFTRH-IZFDCT)
C
        GZFDCT=LKFDCT  
C
        IF (LKFDCT.NE.0.AND.ITRACK.NE.0) 
     &    GZFDCT=LZFIND(IXCOM,LKFDCT,ITRACK,-5)
      END IF
C----------------------------------------------------------
      RETURN
      END
