      INTEGER FUNCTION GZHITS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Get pointer to HITS bank
C-      PATHGZ returns pointer for RECO, GEAN or FAKE
C-      depending on path chosen with PATHST
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-   Updated   6-JAN-1996   Bob Hirosky  bugger to work w/ QMS_DATA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHITS.LINK'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INTEGER LSUP,PATHGZ,LRECO,GZRECO
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
      GZHITS=0
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN ! maybe it's QMS_DATA?
        LRECO = GZRECO(1)
        IF (LRECO.GT.0) GZHITS=LQ(LRECO-IZHITS)
      ELSE
        LSUP=PATHGZ()
        IF(LSUP.GT.0) GZHITS=LQ(LSUP-IZHITS)
      ENDIF
  999 RETURN
      END
