      SUBROUTINE FMARK_TRKHITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set status bits for FTRAKS FDC track banks.
C-                         Also set status flags in FDCT.
C-                         (Replaces FRENUM).
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-AUG-1991   Robert E. Avery
C-      based on FRENUM:  Created  23-JAN-1991   Jeffrey Bantly
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated   3-JUL-1992   Robert E. Avery   Set status flags in FDCT.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ITRACK
      INTEGER GOOD_TRACKS
      INTEGER ILYR,LADDER(0:2),MODULE,HALF
      INTEGER LKFDCT,LKFTRH
      INTEGER GZFDCT,GZFTRH
      INTEGER STATUS,FTRK_QUALITY
C
C----------------------------------------------------------------------
      LKFTRH=GZFTRH()
      IF(LKFTRH.LE.5) GOTO 999
      GOOD_TRACKS=IQ(LKFTRH+2)
C
C  Mark hits in hit banks with segment number and LR ambiguity used.
C
      DO 10 ITRACK=1,GOOD_TRACKS
        LKFDCT=GZFDCT(ITRACK)
        IF(LKFDCT.LE.5) GOTO 10
        HALF=IAND(1,IQ(LKFDCT+1))
        CALL FGETLDR2(ITRACK,LADDER)
        DO 20 ILYR=0,2
          IF(LADDER(ILYR).EQ.0) GOTO 20
          MODULE=HALF*3+ILYR
          CALL FMARK_SEGHITS(MODULE,LADDER(ILYR),ITRACK,2)
   20   CONTINUE
C
        STATUS = FTRK_QUALITY(ITRACK)
        IQ(LKFDCT+1) = IOR(IQ(LKFDCT+1),STATUS)
   10 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
