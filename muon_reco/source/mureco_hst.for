      LOGICAL FUNCTION MURECO_HST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill histograms for MUON-RECO
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-MAR-1990   Shuichi Kunori
C-    9-91 DH add call for muon-calib
C     1-92 DH use VERIFY flag
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,FLGVAL
      INTEGER IERR,ID0,ID1,IDUSER,MUCALIB
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      MURECO_HST=.TRUE.
C
      CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MURECO','MURECO_HST',
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C  Book histograms.
C  =================
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         ID0=-1
         ID1=-1
         IDUSER=-1
         CALL EZPICK('MURECO_RCP')
         CALL EZGET('HIST_MINIMUM',ID0,IERR)
         CALL EZGET('HIST_SET1',ID1,IERR)
         CALL EZGET('HIST_USER',IDUSER,IERR)
         CALL EZGET('MUCALIB',MUCALIB,IERR)
         CALL EZRSET
      ENDIF
C
C  Fill histograms.
C  ================
C
      IF(ID0.GE.0) THEN
         CALL MUHIST_MINI(ID0)
      ELSE IF(FLGVAL('VERIFY')) THEN
         CALL MUHIST_MINI(0)
      ENDIF
C
      IF(ID1.GE.0) THEN
         CALL MUHIST_SET1(ID1)
      ELSE IF(FLGVAL('VERIFY')) THEN
         CALL MUHIST_SET1(0)
      ENDIF
C
      IF(IDUSER.GE.0) THEN
         CALL MUUSER_HIST(IDUSER)
      ENDIF
      IF(MUCALIB.GE.0) THEN
         CALL MUCALIB_EVT(MUCALIB)
      ENDIF
C
  999 RETURN
      END
