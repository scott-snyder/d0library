      SUBROUTINE MUGMER(IPOINT,XYZER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS error (cm) due to geometry for a
C-   hit in the MUOH bank
C-   Inputs  : IPOINT = which MUOH hit
C-             IDRFT  = which drift solution 
C-   Controls: 
C-
C-   Created  26-AUG-1992   David Hedin
C-  extremely preliminary version
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IPOINT,LMUOH,GZMUOH,IADD
      REAL XYZER(3)
      XYZER(1)=99999.
      XYZER(2)=99999.
      XYZER(3)=99999.
      LMUOH=GZMUOH(0)
      IF(LMUOH.EQ.0) GO TO 999
      IADD=IQ(LMUOH+(IPOINT-1)*28+1)
      XYZER(1)=.2
      XYZER(2)=.2
      XYZER(3)=.2
C----------------------------------------------------------------------
  999 RETURN
      END
