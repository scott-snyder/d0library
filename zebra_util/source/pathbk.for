      SUBROUTINE PATHBK(LSUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book supporting bank depending on chosen path
C-     CALL PATHST(PATH) to chose a path (GEAN, RECO, FAKE or FILT)
C-
C-   Outputs :
C-   LSUP = supporting link
C-
C-   Created  28-JUN-1989   Serban D. Protopopescu
C-   Updated  11-MAR-1992   James T. Linnemann  check path before booking
C-   Updated  20-MAR-1992   Andrew J. Milder  Add MicroDST path 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSUP,PATHGZ
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
      LSUP = PATHGZ()
      IF(LSUP.LE.0) THEN
        CALL PATHGT(PATH)
        IF(PATH.EQ.'RECO') CALL BKRECO(LSUP)
        IF(PATH.EQ.'GEAN') CALL BKGEAN(LSUP)
        IF(PATH.EQ.'FAKE') CALL BKFAKE(LSUP)
        IF(PATH.EQ.'FILT') CALL BKFILT(LSUP)
        IF(PATH.EQ.'MDST') CALL BKANLS(LSUP)
      ENDIF
      IF(LSUP.EQ.0)
     &  CALL ERRMSG('No path for headers','PATHBK',
     &  PATH//' is not a valid path','F')
  999 RETURN
      END
