      SUBROUTINE CDST_ANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Analyze standard histograms from a DST file 
C-   CALLED FROM CAL_EVENT
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-NOV-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL FIRST,DO_CHTANL,DO_C3PMET_ANL,DO_CJTANL,DO_CPHANL,
     &  DO_CTAUS_ANL
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('DO_ANALYSIS',DO_CHTANL,IER)
        CALL EZRSET
C
        CALL EZPICK('C3PMET_RCP')
        CALL EZGET('DO_ANALYSIS',DO_C3PMET_ANL,IER)
        CALL EZRSET
C
        CALL EZPICK('CAJETS_RCP')
        CALL EZGET('DO_ANALYSIS',DO_CJTANL,IER)
        CALL EZRSET
C
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('DO_ANALYSIS',DO_CPHANL,IER)
        CALL EZRSET
C
        CALL EZPICK('CTAUS_RCP')
        CALL EZGET('DO_ANALYSIS',DO_CTAUS_ANL,IER)
        CALL EZRSET
C
      ENDIF
C
      CALL DHDIR('CDST_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CDST_ANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(DO_CHTANL)CALL CHTANL
      IF(DO_C3PMET_ANL)CALL C3PMET_ANL
      IF(DO_CJTANL)CALL CJTANL
      IF(DO_CPHANL)CALL CPHANL
      IF(DO_CTAUS_ANL)CALL CTAUS_ANL
C
  999 RETURN
      END
