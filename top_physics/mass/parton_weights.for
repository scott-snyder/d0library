      SUBROUTINE PARTON_WEIGHTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WORK OUT PARTON MODEL WEIGHTS FOR CONFIGURATION
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:PARTON_KINE.INC'
C
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER I
      DOUBLE PRECISION    TTBAR_WEIGHTS
      REAL    RNDM
      INTEGER NTEST
      DOUBLE PRECISION    SHT,CTH,DUMMY
      REAL    TMAST
      LOGICAL TESTQ1
C
      REAL    ROOTS_TEV_S,ALPHA_S_S,CONV_PB_S
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('ROOTS_TEV',ROOTS_TEV_S,IER)
        CALL EZGET('QCD_LAMBDA',ALAM,IER)
        CALL EZGET('ALPHA_STRONG',ALPHA_S_S,IER)
        ALAM2 = ALAM*ALAM
        CALL EZGET('CUTJET',CUTJET,IER)
        CALL EZGET_i('STRUCTURE_FUNCTION_TYPE',ISTRUC,IER)
C
        CALL EZGET_l('TEST_MODEL',TESTQ,IER)
        CALL EZGET_l('TEST_MODEL_IN_SITU',TESTQ1,IER)
        CALL EZGET_i('NUMBER_TESTS',NTEST,IER)
        CALL EZGET('TOP_TEST_MASS',TMAST,IER)
        CALL EZGET_d('TEST_SHAT',SHT,IER)
        CALL EZGET('CONV_PB',CONV_PB_S,IER)
C
        ROOTS_TEV=ROOTS_TEV_S
        ALPHA_S = ALPHA_S_S
        CONV_PB = CONV_PB_S
C
        PCM = ROOTS_TEV/2.0   !MOMENTUM OF P AND PBAR IN CMS
        CALL EZRSET
        DO I = 1 , 2
          THAT(I) = 0.
          UHAT(I) = 0.
          X1P(I) = 0.
          X2P(I) = 0.
        ENDDO
C
        IF ( TESTQ ) THEN
          TMASS = TMAST
          QSQ = TMAST**2
          SH = SHT
          TAU = SH/ROOTS_TEV**2
          XF = X1-X2
C
C ****  test parton model distributions
C
          DO  I = 1 , NTEST
            X1 = ABS(RNDM(0))
            X2 = TAU/X1
            CTH = 2.*RNDM(0) - 1.0
            TH = -SH*(1.-CTH)/2.
            UH = -SH*(1.+CTH)/2.
            DUMMY = TTBAR_WEIGHTS()
          ENDDO
C
          TESTQ = .FALSE.
        ENDIF
        IF ( TESTQ1 ) THEN
          TESTQ = .TRUE.   !WILL HISTOGRAM IN SITU
        ENDIF
      ENDIF
C
      DO IS2  = 1 , 2
        DO IS1 = 1 , 2 !1ST SOLUTION OUT 1ST
          IF ( SOL_MIN(IS1,IS2) ) THEN
            TMASS = 0.5*(TOP1_MIN(5,IS1,IS2)+TOP2_MIN(5,IS1,IS2))
            QSQ = TMASS**2
            CALL PAIR_MASSD(TOP1_MIN(1,IS1,IS2),
     &        TOP2_MIN(1,IS1,IS2),SHAT)
            TAU = (SHAT(5)/ROOTS_TEV)**2
            XF = SHAT(3)/PCM  !FEYNMAN X OF T-TBAR SYSTEM
            ROOT =  (XF*XF+4.0*TAU)
            IF ( ROOT.GT.0.0 ) THEN
              ROOT = DSQRT(ROOT)
            ELSE
              ROOT = 0.0
            ENDIF
            X1 = (+XF + ROOT)/2.0
            X2 = (-XF + ROOT)/2.0
            X1P(3) = X1*PCM
            X1P(4) = ABS(X1P(3))  !ENERGY = MOMENTUM. NO TRANSVERSE MMTM
            X2P(3) = -X2*PCM
            X2P(4) = ABS(X2P(3))
            DO I = 1 , 4
              X1P(I) = -X1P(I)
              X2P(I) = -X2P(I)   !TO TAKE MOMENTUM TRANSFERS
            ENDDO
            CALL PAIR_MASSD(TOP1_MIN(1,IS1,IS2),X1P,THAT)
            CALL PAIR_MASSD(TOP2_MIN(1,IS1,IS2),X1P,UHAT)
C
C ****  NOW TO GET SHAT,UHAT,THAT
C
            TMASS = 0.5*(TOP1_MIN(5,IS1,IS2)+TOP2_MIN(5,IS1,IS2))
C
            SH = SHAT(5)**2
            TH = SIGN(THAT(5)**2,THAT(5))
            UH = SIGN(UHAT(5)**2,UHAT(5))
C
            WEIGHT(IS1,IS2) = TTBAR_WEIGHTS()
C
            CALL DO_HF1D(106,X1,WEIGHT(IS1,IS2))
            CALL DO_HF1D(107,X2,WEIGHT(IS1,IS2))
            CALL DO_HF1D(108,ABS(THAT(5)),WEIGHT(IS1,IS2))
            CALL DO_HF1D(109,L1,WEIGHT(IS1,IS2))
            CALL DO_HF1D(110,L2,WEIGHT(IS1,IS2))
          ENDIF
C
        ENDDO
      ENDDO
C
  999 RETURN
      END
