      SUBROUTINE GTFDCT(ITRACK,QTRAK,QHSEC,LADDER)
C-----------------------------------------------------------------------
C
C    Purpose and Methods : Returns a FDC track and associated hits
C
C    Input  : ITRACK     = FDC track number
C
C    Output : QTRAK(1:22)= contains information on the fitted track
C             QHSEC      = contains bank location info of hits on track
C             LADDER(0:2)= contains the segment ladder of segs on track
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  xx-DEC-1989   Daria Zieminska  call GTFDCT_LINK
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-   Updated  23-JUL-1990   Jeffrey Bantly  add theta angle to track 
C-   Updated  24-JAN-1991   Jeffrey Bantly  add segment ladder 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRACK,LOC,LADDER(0:2)
      INTEGER GZFDCT
C
      REAL QTRAK(26),QHSEC(3,34)
C------------------------------------------------------------------------
C
      LOC=GZFDCT(ITRACK)
      CALL GTFDCT_LINK(LOC,QTRAK,QHSEC,LADDER)
C
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
