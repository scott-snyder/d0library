      LOGICAL FUNCTION SAMRECO_HST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill histograms for SAMUS-RECO
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAY-1991   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER IERR,ID0,ID1,IDUSER
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      SAMRECO_HST = .TRUE.
C
      CALL DHDIR('SAMRECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('SAMRECO','SAMRECO_HST',
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
         CALL EZPICK('SAMRECO_RCP')
         CALL EZGET('HIST_MINIMUM',ID0,IERR)
         CALL EZGET('HIST_USER',IDUSER,IERR)
         CALL EZRSET
      ENDIF
C
C  Fill histograms.
C  ================
C
      IF(ID0.GE.0) THEN
         CALL SAMHIST_MINI(ID0)
      ENDIF
C
      IF(IDUSER.GE.0) THEN
         CALL SAMUSER_HIST(IDUSER)
      ENDIF
C
  999 RETURN
      END
