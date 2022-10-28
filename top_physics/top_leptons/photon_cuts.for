      SUBROUTINE PHOTON_CUTS(PHOT_PASSED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PHOT.INC'
C
      REAL    ETCUT,ISOLCUT,MSIG_CUT(2)
      REAL    CORR,ETCORR,CHISQ,DET_ETA,MSIGCUT,FISO,FEM,MATCH_SIG
      REAL    FLG_CSQE
      INTEGER IER,N,I,PHOTN
      INTEGER PHOT_PASSED,NFAKE_PHOT,TSTCSQ,LEVEL
      LOGICAL REJ_MBLANK,REQ_TRKMATCH,REQ_CSQCUT,FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('ETCUT',ETCUT,IER)
        CALL EZGET_i('LEVEL',LEVEL,IER)
        CALL EZGET('ISOLCUT',ISOLCUT,IER)
        CALL EZGET_l('REQ_CSQCUT',REQ_CSQCUT,IER)
        CALL EZGET_l('REJ_MBLANK',REJ_MBLANK,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C ** Initiaize variables
C
      DO I = 1,MAX_PHO
        IGPHOT(I) = 0
      ENDDO
      PHOT_PASSED = 0
C
      DO I = 1,NPHOT
        CORR = RPHOT(10,I)
        ETCORR = RPHOT(1,I)*CORR
        CHISQ = RPHOT(6,I)
        DET_ETA = ABS(RPHOT(5,I))
        FLG_CSQE  = RPHOT(7,I)
        FISO = RPHOT(8,I)
        FEM   = RPHOT(11,I)
C
C ** Make Cuts here
C
        IF(ETCORR .LT. ETCUT)GOTO 100
        IF(FISO .GT. ISOLCUT)GOTO 100
        IF(FEM  .LE. 0.9)GOTO 100
        IF(DET_ETA .GT. 25)GOTO 100
        IF(REQ_CSQCUT)THEN
          IF(FLG_CSQE .NE. 0.)GOTO 100
        ENDIF
C
        PHOT_PASSED = PHOT_PASSED+1
        IGPHOT(PHOT_PASSED) = I
C
  100   CONTINUE
      ENDDO
      NPHOT = PHOT_PASSED
C
  999 RETURN
      END
