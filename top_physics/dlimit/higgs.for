      SUBROUTINE HIGGS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DO A CHARGED HIGGS LIKELIHOOD IN TANBETA MASS
C-                         PLANE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-APR-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_CROSS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
      REAL    MLO,MSTEP,TOT_BCKGD,BCKGD_ERR,TMASS
      REAL    XMLO,XMSTEP
      INTEGER XNMASS
      INTEGER I,J,K,NMASS,NEVTOT
      REAL    LUMIN(7)
      REAL    ELUMIN(7)
      INTEGER NEVS(7)
      REAL    XSECT,CROSS_SECT
      REAL    EXSECT,ERRX
      INTEGER NEXPGEN
      INTEGER NCH,IER
      REAL    CROSS,GAUSS,ALUM
      REAL    SBR(7),ESBR(7),BCKGD(7),EBCKGD(7),BR_CHANNEL(7)
      REAL EE_SIGBR,EMU_SIGBR,EJETS_SIGBR,EJETS_TAG_SIGBR,
     &               MUMU_SIGBR,MUJETS_SIGBR,MUJETS_TAG_SIGBR
      REAL EE_SERR,EMU_SERR,EJETS_SERR,EJETS_TAG_SERR,
     &               MUMU_SERR,MUJETS_SERR,MUJETS_TAG_SERR
      REAL    SB,EVEXP(7),BCKEXP(7)
      INTEGER NREP,NREPG,NTOTGEN,NGENEXP(7),NBCKEXP(7)
      REAL    PRB,PRBG
      REAL    MASS_CENT,MASS_ERR,MASS_SYS,LIKE_MASS,SIG_MASS
      REAL    MLO1,MHI1,TBLO1,TBHI1
      REAL    HLO,HMSTEP,HMASS,TBLO,TBSTEP,TB
      REAL    BMASS
      INTEGER NHMASS,NTB
      INTEGER IH,IT
      REAL    H,TAU,W,TAU_E,TAU_MU,W_E,W_MU,W_J,H_E,H_MU,H_J
      REAL    BF(7)
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('DLIMIT_RCP')
C
        CALL EZGET('XMLO',XMLO,IER)
        CALL EZGET_i('XNMASS',XNMASS,IER)
        CALL EZGET('XMSTEP',XMSTEP,IER)
C
        CALL EZGET('MLO',MLO,IER)
        CALL EZGET_i('NMASS',NMASS,IER)
        CALL EZGET('MSTEP',MSTEP,IER)
C
        CALL EZGET_i('NEVTOT',NEVTOT,IER)
        CALL EZGET('TOT_BCKGD',TOT_BCKGD,IER)
        CALL EZGET('BCKGD_ERR',BCKGD_ERR,IER)
        CALL EZGET_rarr('LUMIN',LUMIN,IER)
        CALL EZGET_rarr('ELUMIN',ELUMIN,IER)
        CALL EZGET_rarr('BCKGD',BCKGD,IER)
        CALL EZGET_rarr('EBCKGD',EBCKGD,IER)
        CALL EZGET_iarr('NEVS',NEVS,IER)
        CALL EZGET('EXSECT',EXSECT,IER)
        CALL EZGET_i('NEXPGEN',NEXPGEN,IER)
        CALL EZGET('MASS_CENT',MASS_CENT,IER)
        CALL EZGET('MASS_ERR',MASS_ERR,IER)
        CALL EZGET('MASS_SYS',MASS_SYS,IER)
        SIG_MASS = SQRT(MASS_ERR**2+MASS_SYS**2)
C
        CALL EZGET_rarr('BR_CHANNEL',BR_CHANNEL,IER)
        CALL EZGET('BMASS',BMASS,IER)
        CALL EZGET('HLO',HLO,IER)
        CALL EZGET_i('NHMASS',NHMASS,IER)
        CALL EZGET('HMSTEP',HMSTEP,IER)
C
        CALL EZGET('TBLO',TBLO,IER)
        CALL EZGET_i('NTB',NTB,IER)
        CALL EZGET('TBSTEP',TBSTEP,IER)
        CALL EZGET('TAU_E',TAU_E,IER)
        TAU_MU=TAU_E   !BR TAU TO E
        CALL EZGET('W_E',W_E,IER)
        W_MU = W_E      !BR W TO E
        CALL EZGET('W_J',W_J,IER)
C
        CALL EZRSET
C
        NCH = 7   ! NUMBER OF CHANNELS
C
        MLO1 = HLO-0.5*HMSTEP
        MHI1 = MLO1 + NHMASS*HMSTEP

        TBLO1 = TBLO - 0.5*TBSTEP
        TBHI1 = TBLO1 + NTB*TBSTEP
C
        CALL HBOOK2(101,' TAN BETA VS HIGGS MASS LIKELIHOOD ',
     &    NTB,TBLO1,TBHI1,NHMASS,MLO1,MHI1,0.)
        CALL HBOOK2(102,' TAN BETA VS HIGGS MASS TOP TO HIGGS BR ',
     &    NTB,TBLO1,TBHI1,NHMASS,MLO1,MHI1,0.)
        CALL HBOOK2(103,
     &    ' TAN BETA VS HIGGS MASS LIKELIHOOD HIGGS to TAU BR ',
     &    NTB,TBLO1,TBHI1,NHMASS,MLO1,MHI1,0.)
C
      ENDIF
C
      DO I = 1 , XNMASS
        TMASS = GAUSS(MASS_CENT,SIG_MASS,MLO)
        LIKE_MASS = 1.0
        IEN = 18
        XSECT = CROSS_SECT(TMASS)  !CENTRAL VALUE OF CROSS WSECTION
        ERRX = XSECT*EXSECT
C
        DO K = 1 , NCH
          IF ( K.EQ.1 ) THEN
            SBR(K) = EMU_SIGBR(TMASS)
            ESBR(K) = EMU_SERR(TMASS)
          ELSEIF ( K.EQ.2 ) THEN
            SBR(K) = EE_SIGBR(TMASS)
            ESBR(K) = EE_SERR(TMASS)
          ELSEIF ( K.EQ.3 ) THEN
            SBR(K) = MUMU_SIGBR(TMASS)
            ESBR(K) = MUMU_SERR(TMASS)
          ELSEIF ( K.EQ.4 ) THEN
            SBR(K) = EJETS_SIGBR(TMASS)
            ESBR(K) = EJETS_SERR(TMASS)
          ELSEIF ( K.EQ.5 ) THEN
            SBR(K) = MUJETS_SIGBR(TMASS)
            ESBR(K) = MUJETS_SERR(TMASS)
          ELSEIF ( K.EQ.6 ) THEN
            SBR(K) = EJETS_TAG_SIGBR(TMASS)
            ESBR(K) = EJETS_TAG_SERR(TMASS)
          ELSEIF ( K.EQ.7 ) THEN
            SBR(K) = MUJETS_TAG_SIGBR(TMASS)
            ESBR(K) = MUJETS_TAG_SERR(TMASS)
          ENDIF
          SBR(K) = SBR(K)/BR_CHANNEL(K)    !EFFICIENCY OF CHANNEL
          ESBR(K) = ESBR(K)/BR_CHANNEL(K)  !ERROR EFFICIENCY OF CHANNEL
        ENDDO
        DO IH = 1 , NHMASS
          HMASS = HLO + (IH-1)*HMSTEP  !HIGGS MASS
          IF ( HMASS . LT. (TMASS-BMASS) ) THEN
            DO IT = 1 , NTB
              TB = TBLO + (IT-1)*TBSTEP
C
              CALL BRATIOS(TMASS,HMASS,TB,H,TAU)
              W = 1-H  !BRANCHING RATIO OF TOP TO W.
              IF ( I.EQ.1 ) THEN
                CALL HFILL(102,TB,HMASS,H)
                CALL HFILL(103,TB,HMASS,TAU)
              ENDIF
C
              H_E = TAU*TAU_E
              H_MU = TAU*TAU_MU
              H_J = (1-TAU)
C
              BF(1) = 2.0*(W*W*W_E*W_MU+W*H*(W_E*H_MU+W_MU*H_E) + 
     &          H*H*H_E*H_MU)   !EMU BRANCHING FRACTION
C
              BF(2) = W*W*W_E*W_E + 2.0*W*H*W_E*H_E  !EE BFR
C
              BF(3) = W*W*W_MU*W_MU + 2.0*W*H*W_MU*H_MU  !MUMU BFR
C
              BF(4) = 2.0*(W*W*W_E*W_J+W*H*(W_E*H_J+W_J*H_E) + 
     &          H*H*H_E*H_J)   !E_J BRANCHING FRACTION
C
              BF(5) = 2.0*(W*W*W_MU*W_J+W*H*(W_MU*H_J+W_J*H_MU) + 
     &          H*H*H_MU*H_J)   !MU_J BRANCHING FRACTION
C
              BF(6) = BF(4)   !EJ TAG
              BF(7) = BF(5)   !MUJ TAG
C
              DO K = 1 , NCH 
                SBR(K) = SBR(K)*BF(K)  !EFFICIENCY * BRANCHING RATIO
                ESBR(K) = ESBR(K)*BF(K)  !EFFICIENCY * BRANCHING RATIO
C ASSUMING HERE THAT HIGGS AND W EFFICIENCIES ARE THE SAME. MODIFY LATER
              ENDDO
C
              NREP = 0
              NREPG = 0
              DO J = 1 , NEXPGEN
C
C ****  GENERATE THESE MANY EXPERIMENTS AT THIS MASS AND ASK HOW OFTEN
C ****  TOTAL NUMBER OF EVENTS OBSERVED = ACTUAL NUMBER.
C ****  HISTOGRAM MASS WITH THIS PROBABILITY AS WEIGHT.
C ****  FIND MAXIMUM LIKELIHOOD MASS
C
                CROSS = GAUSS(XSECT,ERRX,0.0)
                NTOTGEN = 0 !TOTAL EVENTS GENERATED
                DO K = 1 , NCH
                  ALUM = GAUSS(LUMIN(K),ELUMIN(K),0.0)
                  SB   = GAUSS(SBR(K),ESBR(K),0.0)
                  EVEXP(K) = ALUM*CROSS*SB
                  CALL POISSN(EVEXP(K),NGENEXP(K),IER)
                  IF ( IER.NE.0.AND.IER.NE.1 ) THEN
                    CALL ERRMSG(' DLIMIT',' HIGGS ',
     &                ' ERROR IN POISSN ','W')
                  ENDIF
                  BCKEXP(K) = GAUSS(BCKGD(K),EBCKGD(K),0.0)  !BACKGROUND
                  CALL POISSN(BCKEXP(K),NBCKEXP(K),IER)
                  NTOTGEN = NGENEXP(K) + NBCKEXP(K) + NTOTGEN
                ENDDO
                IF ( NTOTGEN.EQ.NEVTOT ) THEN
                  NREP = NREP + 1 !NUMBER OF EXPTS WITH EVENTS=OBSERVED
                ENDIF
                IF ( NTOTGEN.GE.NEVTOT ) THEN
                  NREPG = NREPG + 1 !NUMBER OF EXPTS WITH GE NEVTOT
                ENDIF
C                CALL DO_HF1(21,FLOAT(NTOTGEN),LIKE_MASS)
              ENDDO
              PRB = FLOAT(NREP)/FLOAT(NEXPGEN)
              PRBG = FLOAT(NREPG)/FLOAT(NEXPGEN)
              CALL DO_HF2(101,TB,HMASS,PRB)
            ENDDO
          ENDIF
        ENDDO
C
      ENDDO
C
  999 RETURN
      END
