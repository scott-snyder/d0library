      SUBROUTINE FITFDC_VERTEX(LFDCT,ZVERT,QTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control fitting of an FDCT track with the 
C-    vertex point.
C-
C-   Inputs  : LFDCT = link to FDCT bank
C-   Outputs : QTRAK,IQTRAK = track information
C-   Controls: 
C-
C-   Created   9-MAR-1992   Susan K. Blessing
C-   Updated  22-APR-1993   Susan K. Blessing  Add ZVERT to call to 
C-    identify which vertex to fit to.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LFDCT
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER HALF,LADDER(0:2)
      INTEGER TRACK
      INTEGER IVERT
      INTEGER DUM
C
      REAL QTRAK(26),QHSEC(3,34)
      REAL CHINORM
      REAL ZVERT
C
C----------------------------------------------------------------------
C
C Get track information
      TRACK = IQ(LFDCT-5)
      CALL GTFDCT(TRACK,QTRAK,QHSEC,LADDER)
C
      CALL UCOPY(QTRAK(1),DUM,1)
      HALF = IBITS(DUM,0,1)
C
C Figure out which vertex
      CALL FGETVERT(ZVERT,IVERT)
C
      CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,
     &  CHINORM,IVERT)
C
  999 RETURN
      END
