C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFDCT_COS.FOR
C *1     4-NOV-1993 10:52:12 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element BKFDCT_COS.FOR
      SUBROUTINE BKFDCT_COS(LKFDCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book an FDCT FDC Track bank. Special version for 
C-      cosmic ray format. Extra words for t0 and associated parameters. 
C-
C-   Outputs : LKFDCT = FDCT track bank link
C-
C-   Created  20-JAN-1991   Jeffrey Bantly
C-   Updated  17-SEP-1991   Susan K. Blessing  Add four floating point
C-    words.  Two for theta and phi errors and two spares.
C-   Updated  28-OCT-1991   Susan K. Blessing  Add a reference link 
C-    to the ZTRK bank.
C-   Updated  10-MAR-1992   Susan K. Blessing  Change +25 word to integer 
C-   Updated   1-NOV-1993   Robert E. Avery  Special version for 
C-      cosmic ray format. Extra words for t0 and associated parameters. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
      INTEGER IXFDCT
      INTEGER LKFTRH
      INTEGER LFDCT,LKFDCT
      INTEGER GZFTRH
C
      LOGICAL FIRST
C
      SAVE FIRST,IXFDCT
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------
      IF (FIRST) THEN
        CALL MZFORM('FDCT','1B 1I 1B 21F 1I 7F',IXFDCT)
        FIRST=.FALSE.
      ENDIF
      LKFDCT=0
C
      LKFTRH=GZFTRH()
      IF (LKFTRH.LE.0) CALL BKFTRH(LKFTRH) 
      IF (LKFTRH.LE.0) GOTO 999
C
      IQ(LKFTRH+2)=IQ(LKFTRH+2)+1     ! increment number of tracks in FDC
C
      CALL MZBOOK(IXMAIN,LFDCT,LKFTRH,-IZFDCT,'FDCT',2,1,32,IXFDCT,0)
      LKFDCT=LFDCT
C
C----------------------------------------------------------------------
  999 RETURN
      END
