      SUBROUTINE CALCULATE_WEIGHTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Dalitz and correct jacobian weights
C-   for configuration.
C-
C-   Inputs  : NTOPS IS SET TO LATEST TOP MASS INDEX
C-   Outputs :
C-   Controls:
C-
C-   Created   5-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
C
      INTEGER STR_FN_NPTYPE,STR_FN_NGROUP,STR_FN_NSET
      INTEGER STR_FN_NFL,STR_FN_LO
      REAL    STR_FN_SCALE
C
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VAL(20)
C
      DOUBLE PRECISION DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,DGL
      DOUBLE PRECISION DXA,DXB,FAG,FBG,FAQ,FBQ
      DOUBLE PRECISION    DSIGMA_DT_QUARK,DSIGMA_DT_GLUON
      DOUBLE PRECISION    DW,DECAY_WEIGHTS
      DOUBLE PRECISION ALPHAS2
      REAL    KT2W,KT2_FUN
      REAL    KT2
      LOGICAL DO_KT2_WEIGHTS
C
      REAL    T1S(4),T2S(4)
C
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER I,J
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('WMASS',WMASS,IER)
        CALL EZGET('ROOTS_TEV',ROOTS_TEV,IER)
        CALL EZGET('QCD_LAMBDA',ALAM,IER)
        CALL EZGET('ALPHA_STRONG',ALPHA_S,IER)
        ALAM2 = ALAM*ALAM
C
        CALL EZGET('STRUCTURE_FN_NPTYPE',STR_FN_NPTYPE,IER)
        CALL EZGET('STRUCTURE_FN_NGROUP',STR_FN_NGROUP,IER)
        CALL EZGET('STRUCTURE_FN_NSET',STR_FN_NSET,IER)
C
        CALL EZGET('STRUCTURE_FN_NFL',STR_FN_NFL,IER)
        CALL EZGET('STRUCTURE_FN_LO',STR_FN_LO,IER)
        CALL EZGET('STRUCTURE_FN_SCALE',STR_FN_SCALE,IER)
        CALL EZGET('CONV_PB',CONV_PB,IER)
        CALL EZGET('DO_KT2_WEIGHTS',DO_KT2_WEIGHTS,IER)
        CALL EZRSET
C
        PARM(1) = 'NPTYPE'
        VAL(1) = STR_FN_NPTYPE
        PARM(2) = 'NGROUP'
        VAL(2) = STR_FN_NGROUP
        PARM(3) = 'NSET'
        VAL(3) = STR_FN_NSET
        PARM(4) = 'NFL'
        VAL(4) = STR_FN_NFL
        PARM(5) = 'LO'
        VAL(5) = STR_FN_LO
C
        CALL PDFSET(PARM,VAL)
C
        PCM = ROOTS_TEV/2.0   !MOMENTUM OF P AND PBAR IN CMS
C
        DO I = 1 , 2
          THAT(I) = 0.
          UHAT(I) = 0.
          X1P(I) = 0.
          X2P(I) = 0.
        ENDDO
C
      ENDIF
C
      TMASS = TOP_MASS(NTOPS)
      QSQ = TMASS**2
      DSCALE = STR_FN_SCALE*TMASS  !SCALE FOR STRUCUTRE FUNCTIONS
C
      ALPHA_S = ALPHAS2(DSCALE)
C
      DO I = 1 , 4
        RWEIGHT(I,NTOPS) = 0.0
        WEIGHTD(I,NTOPS)=0.
      ENDDO
C
      DO I = 1 , NSOLS(NTOPS)
        CALL PAIR_MASSD(T1(1,I,NTOPS),T2(1,I,NTOPS),SHAT)
        KT2 = SHAT(1)**2 + SHAT(2)**2  !PT**2 OF TTBAR PAIR
        TAU = (SHAT(5)/ROOTS_TEV)**2
        XF = SHAT(3)/PCM  !FEYNMAN X OF T-TBAR SYSTEM
        ROOT =  (XF*XF+4.0*TAU)
        IF ( ROOT.GT.0.0 ) THEN
          ROOT = SQRT(ROOT)
        ELSE
          ROOT = 0.0
        ENDIF
        X1 = (+XF + ROOT)/2.0
        X2 = (-XF + ROOT)/2.0
        X1P(3) = X1*PCM
        X1P(4) = ABS(X1P(3))  !ENERGY = MOMENTUM. NO TRANSVERSE MMTM
        X2P(3) = -X2*PCM
        X2P(4) = ABS(X2P(3))
        DO J = 1 , 4
          X1P(J) = -X1P(J)
          X2P(J) = -X2P(J)   !TO TAKE MOMENTUM TRANSFERS
        ENDDO
        CALL PAIR_MASSD(T1(1,I,NTOPS),X1P,THAT)
        CALL PAIR_MASSD(T2(1,I,NTOPS),X1P,UHAT)
C
C ****  NOW TO GET SHAT,UHAT,THAT
C
        SH = SHAT(5)**2
        TH = SIGN(THAT(5)**2,THAT(5))
        UH = SIGN(UHAT(5)**2,UHAT(5))
C
        DXA = X1
        DXB = X2
C
        IF ( ABS(X1).LT.1.0.AND.ABS(X2).LT.1.0 ) THEN
          CALL STRUCTF(DXA,DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,
     &      DGL)
          FAQ = (DUPV+DDNV)/DXA
          FAG = DGL/DXA
C
          CALL STRUCTF(DXB,DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,
     &      DGL)
          FBQ = (DUPV+DDNV)/DXB
          FBG = DGL/DXB
C
        ELSE
          FAQ=0.0D0
          FAG=0.0D0
          FBQ=0.0D0
          FBG=0.0D0
        ENDIF
C
        DW = DECAY_WEIGHTS()
C
        IF ( DO_KT2_WEIGHTS ) THEN
          KT2W = KT2_FUN(KT2)
        ELSE
          KT2W = 1.0
        ENDIF
C
C        WEIGHTD(I,NTOPS) = (FAQ)*(FBQ)*DW*KT2W   !DALITZ WEIGHT
C        RWEIGHT(I,NTOPS) = (FAQ*FBQ*DW*KT2W)/TOP_CROSS(NTOPS)
C
        WEIGHTD(I,NTOPS) = (FAG+FAQ)*(FBG+FBQ)*DW*KT2W   !DALITZ WEIGHT
        RWEIGHT(I,NTOPS) = (FAG*FBG*DSIGMA_DT_GLUON(TH)+
     &    FAQ*FBQ*DSIGMA_DT_QUARK(TH))*DW*KT2W/TOP_CROSS(NTOPS)
C
C PROPER Raja/Strovink Weight. NEEDS Jacobian
C
        WEIGHTD(I,NTOPS) = WEIGHTD(I,NTOPS)/NCNFGE
        RWEIGHT(I,NTOPS) = RWEIGHT(I,NTOPS)/NCNFGE
C normalized to the number of configurations generated
      ENDDO
  999 RETURN
      END
