      INTEGER FUNCTION PATHGZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Get pointer to supporting bank depending on path
C-
C-   Created   27-JUN-1989   Serban D. Protopopescu
C-   Updated  13-AUG-1991   Andrew J. Milder  Add MicroDST (ANLS,MDST) path 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZRECO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFAKE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFILT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZANLS.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMDST.LINK/LIST'
      CHARACTER*4 PATH
      INTEGER LSUP,LANLS
C----------------------------------------------------------------------
C
      LSUP=0
      CALL PATHGT(PATH)
      IF(PATH.EQ.'MDST') THEN
        LANLS=LQ(LHEAD-IZANLS)
        LSUP =LQ(LANLS-IZMDST)
      ENDIF
      IF(PATH.EQ.'GEAN') LSUP=LQ(LHEAD-IZGEAN)
      IF(PATH.EQ.'RECO') LSUP=LQ(LHEAD-IZRECO)
      IF(PATH.EQ.'FAKE') LSUP=LQ(LHEAD-IZFAKE)
      IF(PATH.EQ.'FILT') LSUP=LQ(LHEAD-IZFILT)
      PATHGZ=LSUP
  999 RETURN
      END
