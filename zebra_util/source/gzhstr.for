      INTEGER FUNCTION GZHSTR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Get pointer to HSTR bank
C-      GZPATH returns pointer for RECO, GEAN or FAKE
C-      depending on path chosen with PATHST
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INTEGER LSUP,PATHGZ
C----------------------------------------------------------------------
C
      GZHSTR=0
      LSUP=PATHGZ()
      IF(LSUP.GT.0) GZHSTR=LQ(LSUP-IZHSTR)
  999 RETURN
      END
