      FUNCTION CDST_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the RCP files
C-                         in all packages so that standard histograms
C-                         can be made without the package itself being run.
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-NOV-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CDST_INI
      LOGICAL VERINI,MURECO_INI,C3PMET_INI,CJTINI,CPHINI
      LOGICAL CTAUS_INI,ZTRINI,CHTINI
      LOGICAL FIRST,DO_CHTANL,DO_C3PMET_ANL,DO_CJTANL,DO_CPHANL,
     &  DO_CTAUS_ANL
      LOGICAL DO_CAHITS,DO_CAPHEL,DO_CAJETS,DO_ZTRAKS
      LOGICAL DO_CTAUS,DO_C3PMET,DO_MURECO
      DATA FIRST/.TRUE./
      INTEGER IER
      LOGICAL USE_DBL3
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C       read in files
        CDST_INI = .FALSE.
        CALL INRCP('CDST_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
        CALL INRCPE('CDST_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('CALORIMETER','CDST_INI',
     &  ' Default CDST_RCP modified','W')

        CDST_INI = .TRUE.
      ENDIF
C
      CALL EZPICK('CALFRAME_RCP')
      CALL EZGET('DO_MURECO',DO_MURECO,IER)
      CALL EZGET('DO_C3PMET',DO_C3PMET,IER)
      CALL EZGET('DO_CAJETS',DO_CAJETS,IER)
      CALL EZGET('DO_CAPHEL',DO_CAPHEL,IER)
      CALL EZGET('DO_CAHITS',DO_CAHITS,IER)
      CALL EZGET('DO_CTAUS',DO_CTAUS,IER)
      CALL EZGET('DO_ZTRAKS',DO_ZTRAKS,IER)
      CALL EZRSET
C
      IF(DO_MURECO)CDST_INI = CDST_INI .AND.MURECO_INI()
      IF(DO_C3PMET)CDST_INI = CDST_INI .AND.C3PMET_INI()
      IF(DO_CAJETS)CDST_INI = CDST_INI .AND.CJTINI()
      IF(DO_CAPHEL)CDST_INI = CDST_INI .AND.CPHINI()
      IF(DO_CTAUS)CDST_INI = CDST_INI .AND.CTAUS_INI()
      IF(DO_ZTRAKS)CDST_INI = CDST_INI .AND.ZTRINI()
      IF(DO_CAHITS)CDST_INI = CDST_INI .AND.CHTINI()
C
      CALL EZPICK('CDST_RCP')
      CALL EZGET('USE_DBL3',USE_DBL3,IER)
      CALL EZRSET
C
      IF ( .NOT.USE_DBL3 ) THEN
C
C ****  SUPPRESS DBL3 READING
C
        CALL ERRMSG('CALORIMETER','CDST_INI',
     &    'DBL3 READING BEING SUPRESSED IN TRACKING_RCP FILES','W')
        CALL EZPICK('VTRAKS_RCP')
         CALL EZSET('PD_INI',.FALSE.,IER)
         CALL EZSET('TM_INI',.FALSE.,IER)
         CALL EZSET('GN_INI',.FALSE.,IER)
        CALL EZRSET
C
        CALL EZPICK('TRD_RCP')
         CALL EZSET('CALIB_DATA','NONE',IER)
        CALL EZRSET
C
        CALL EZPICK('DTRAKS_RCP')
         CALL EZSET('PD_INI',.FALSE.,IER)
         CALL EZSET('TM_INI',.FALSE.,IER)
         CALL EZSET('GN_INI',.FALSE.,IER)
        CALL EZRSET
C
        CALL EZPICK('FTRAKS_RCP')
         CALL EZSET('PD_INI',.FALSE.,IER)
         CALL EZSET('TM_INI',.FALSE.,IER)
         CALL EZSET('GN_INI',.FALSE.,IER)
        CALL EZRSET
      ENDIF
C
  999 RETURN
      END
