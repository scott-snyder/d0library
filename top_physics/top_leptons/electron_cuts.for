      SUBROUTINE ELECTRON_CUTS(ELEC_PASSED,NFELEC)
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
      INCLUDE 'D0$INC:ELEC.INC'
C
      REAL    ETCUT,ISOLCUT,MSIG_CUT(2)
      REAL    CORR,ETCORR,CHISQ,DET_ETA,MSIGCUT,FISO,FEM,MATCH_SIG
      REAL    FLG_CSQE
      INTEGER IER,N,I,ELECN
      INTEGER ELEC_PASSED,NFELEC,TSTCSQ,LEVEL
      LOGICAL REJ_MBLANK,REQ_TRKMATCH,REQ_CSQCUT,FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('ETCUT',ETCUT,IER)
        CALL EZGET('LEVEL',LEVEL,IER)
        CALL EZGET('ISOLCUT',ISOLCUT,IER)
        CALL EZGET('REQ_TRKMATCH',REQ_TRKMATCH,IER)
        CALL EZGET('REQ_CSQCUT',REQ_CSQCUT,IER)
        CALL EZGETA('MATCH_SIG_CUT',0,0,0,N,IER)
        CALL EZGETA('MATCH_SIG_CUT',1,N,1,MSIG_CUT,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C ** Initiaize variables
C
      DO I = 1,MAX_ELE
        IGELEC(I) = 0
      ENDDO
      ELEC_PASSED = 0
      NFAKE_ELEC = 0
C
      DO I = 1,NELEC
        CORR = RELEC(10,I)
        ETCORR = RELEC(1,I)*CORR
        CHISQ = RELEC(6,I)
        DET_ETA = ABS(RELEC(5,I))
        FLG_CSQE  = RELEC(7,I) 
        IF(DET_ETA .LE. 12)THEN
          MSIGCUT = MSIG_CUT(1)
        ELSE
          MSIGCUT = MSIG_CUT(2)
        ENDIF
        FISO = RELEC(8,I)
        FEM   = RELEC(11,I)
        MATCH_SIG = RELEC(17,I)
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
        ELEC_PASSED = ELEC_PASSED+1
        IGELEC(ELEC_PASSED) = I
        IF(REQ_TRKMATCH)THEN
          IF(MATCH_SIG .GT. MSIGCUT)THEN
            NFAKE_ELEC = NFAKE_ELEC+1
            IGELEC(ELEC_PASSED) = -I
          ENDIF
        ENDIF
C
  100   CONTINUE
      ENDDO
      NELEC = ELEC_PASSED
      NFELEC = NFAKE_ELEC
C
  999 RETURN
      END
