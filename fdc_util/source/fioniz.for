      SUBROUTINE FIONIZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculate ionization loss for FDC tracks
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JAN-1992   Susan K. Blessing
C-   Updated   6-APR-1992   Susan K. Blessing  HALF was being gotten from
C-    the wrong bit.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LFTRH,GZFTRH
      INTEGER LFDCT,GZFDCT
      INTEGER HALF,LADDER(0:2)
      INTEGER TRACK,NTRACKS
C
      REAL THETA,IONIZATION,IONERR
C
C----------------------------------------------------------------------
C
      LFTRH = GZFTRH()
      IF (LFTRH.LE.5) GOTO 999
      NTRACKS = IQ(LFTRH+2)
C
      DO TRACK = 1, NTRACKS
C
        LFDCT = GZFDCT(TRACK)
        CALL FGETLDR2(TRACK,LADDER)
        HALF = IBITS(IQ(LFDCT+1),0,1)
        THETA = Q(LFDCT+22)
C
C Calculate ionization loss
        CALL FDEDX(HALF,LADDER,THETA,IONIZATION,IONERR)
C
C Fill ionization and error in FDCT bank
        Q(LFDCT+20) = IONIZATION
        Q(LFDCT+21) = IONERR
C
      END DO
C
  999 RETURN
      END
