      REAL FUNCTION MUPDER(IPOINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS error (cm) for a given pad
C-   hit in the MUOH bank
C-   Inputs  : IPOINT = which MUOH hit
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-AUG-1992   David Hedin
C-  extremely preliminary version
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IPOINT,LMUOH,GZMUOH
      REAL PHA,PHB,QQ
      MUPDER=99999.
      LMUOH=GZMUOH(0)
      IF(LMUOH.EQ.0) GO TO 999
      PHA=Q(LMUOH+(IPOINT-1)*28+11)
      PHB=Q(LMUOH+(IPOINT-1)*28+12)
      IF(PHA.GE.4000..OR.PHB.GE.4000.) GO TO 999
      IF(PHA.LE.5..OR.PHB.LE.5.) GO TO 999
      QQ=(PHA-PHB)/(PHA+PHB)
      IF(ABS(QQ).LE..6) THEN
        MUPDER=1.
      ELSE IF(ABS(QQ).GE..8) THEN
        MUPDER=2.
      ELSE
        MUPDER=1.+(ABS(QQ)-.6)/.2
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
