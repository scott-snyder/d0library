      SUBROUTINE ECEMCR_DELTA( IETA3, DE_DELTA, ERR_DE_DELTA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply linearity offset determined from
C-                         Test Beam Load 1 analysis.
C-                         See ECEMCR.DOC and D0 Note 1378 for details.
C-
C-   Inputs  : X_EM3,Y_EM3,Z_EM3_MOD,IETA3   X,Y,Z of cluster on EM3 Mid-plane
C-   Outputs : DE_DELTA            Energy correction to be ADDED to cluster E.
C-   Outputs : ERR_DE_DELTA        Estimated Error in DE_DELTA
C-   Controls:
C-
C-   Created   2-SEP-1992   Anthony L. Spadafora
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST /.TRUE./
      INTEGER IER
      INTEGER ECEM_DELTA_FIRST_IETA,ECEM_DELTA_LAST_IETA,IETA3
      INTEGER FIRST_IETA,LAST_IETA
      PARAMETER( FIRST_IETA  =14,  LAST_IETA=35)

      REAL    DE_DELTA,ERR_DE_DELTA
      REAL    ECEM_DELTA_DE(FIRST_IETA:LAST_IETA)
      REAL    ECEM_DELTA_ERR_DE(FIRST_IETA:LAST_IETA)
      REAL    X_EM3,Y_EM3
      REAL    Z_EM3_MOD
C----------------------------------------------------------------------
C..
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('ECEMCR_RCP')
        CALL EZGET(' ECEM_DELTA_FIRST_IETA ', ECEM_DELTA_FIRST_IETA,IER)
        CALL EZGET(' ECEM_DELTA_LAST_IETA ', ECEM_DELTA_LAST_IETA , IER)
        CALL EZGET('ECEM_DELTA_DE',ECEM_DELTA_DE,IER)
        CALL EZGET('ECEM_DELTA_ERR_DE',ECEM_DELTA_ERR_DE,IER)
        CALL EZRSET
C
C..   Check that number of ietas in RCP file agrees with array dimension
        IF( (ECEM_DELTA_FIRST_IETA.NE.FIRST_IETA) .OR.
     &    (ECEM_DELTA_LAST_IETA .NE.LAST_IETA)  ) THEN
          CALL ERRMSG('CALORIMETER','ECEMCR_DELTA',
     &    'NBINS in RCP DIFFERENT FROM ARRAY DIM - NO corr applied','W')
          IER = -1
          RETURN
        ENDIF
      ENDIF   !first
C
      IF( IETA3.LT. ECEM_DELTA_FIRST_IETA
     &  .OR.IETA3.GT. ECEM_DELTA_LAST_IETA) THEN
        CALL ERRMSG('CALORIMETER','ECEMCR_DELTA',
     &    'CLUSTER IETAD OUT OF RANGE FOR ECEM','W')
        IER = -2
        RETURN
      ENDIF
C
      IER = 0
      DE_DELTA = ECEM_DELTA_DE(IETA3)
      ERR_DE_DELTA = ECEM_DELTA_ERR_DE(IETA3)

  999 RETURN
      END
