      SUBROUTINE CONSISTENCY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks consistency of results with std model
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-FEB-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_CROSS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL first
      SAVE first
      DATA first / .true. /
      REAL    MLO,MSTEP,TOT_BCKGD,BCKGD_ERR,MASS
      REAL    XMLO,XMSTEP
      INTEGER XNMASS
      REAL    BRLO,BRSTEP
      INTEGER NBR
      INTEGER I,IC,J,K,NMASS,NEVTOT
      REAL    LUMIN(7)
      REAL    ELUMIN(7)
      INTEGER NEVS(7)
      REAL    XSECT,CROSS_SECT
      REAL    EXSECT,ERRX
      INTEGER NEXPGEN
      INTEGER NCH,IER
      REAL    CROSS,GAUSS,ALUM
      REAL    SBR(7),ESBR(7),BCKGD(7),EBCKGD(7)
      REAL EE_SIGBR,EMU_SIGBR,EJETS_SIGBR,EJETS_TAG_SIGBR,
     &               MUMU_SIGBR,MUJETS_SIGBR,MUJETS_TAG_SIGBR
      REAL EE_SERR,EMU_SERR,EJETS_SERR,EJETS_TAG_SERR,
     &               MUMU_SERR,MUJETS_SERR,MUJETS_TAG_SERR
      REAL    SB,EVEXP(7),BCKEXP(7)
      INTEGER NREP,NREPG,NTOTGEN,NGENEXP(7),NBCKEXP(7)
      REAL    PRB,PRBG
      REAL    MASS_CENT,MASS_ERR,MASS_SYS,LIKE_MASS,SIG_MASS
      REAL    DEL
      REAL    CLO,CSTEP
      INTEGER NCROSS
      REAL    MLO1,MHI1,CLO1,CHI1
      REAL    BRLO1,BRHI1
      REAL    BRWW
      INTEGER IBR
      REAL    NLIKE
      LOGICAL FLUCTUATE_MASS
      LOGICAL DO_CONTOUR
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('DLIMIT_RCP')
C
        CALL EZGET_l('FLUCTUATE_MASS',FLUCTUATE_MASS,IER)
        CALL EZGET_l('DO_CONTOUR',DO_CONTOUR,IER)
C
        CALL EZGET('BRLO',BRLO,IER)
        CALL EZGET_i('NBR',NBR,IER)
        CALL EZGET('BRSTEP',BRSTEP,IER)
C
        CALL EZGET('XMLO',XMLO,IER)
        CALL EZGET_i('XNMASS',XNMASS,IER)
        CALL EZGET('XMSTEP',XMSTEP,IER)
C
        CALL EZGET('MLO',MLO,IER)
        CALL EZGET_i('NMASS',NMASS,IER)
        CALL EZGET('MSTEP',MSTEP,IER)
        CALL EZGET('CLO',CLO,IER)
        CALL EZGET_i('NCROSS',NCROSS,IER)
        CALL EZGET('CSTEP',CSTEP,IER)
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
        CALL EZRSET
C
        NCH = 7   ! NUMBER OF CHANNELS
C
        MLO1 = XMLO-0.5*XMSTEP
        MHI1 = MLO1 + XNMASS*XMSTEP
C
        BRLO1 = BRLO-0.5*BRSTEP
        BRHI1 = BRLO1 + NBR*BRSTEP
C
        CALL HBOOK1(11,'PROB FOR N=NEV',XNMASS,MLO1,MHI1,0.0)
        CALL HBOOK1(12,'PROB FOR N.GE.NEV',XNMASS,MLO1,MHI1,0.0)
        CALL HBOOK1(13,'PROB FOR N=NEV*MASS LIKE',
     &    XNMASS,MLO1,MHI1,0.0)
        CALL HBOOK1(14,'PROB FOR N.GE.NEV*MASS LIKE',
     &    XNMASS,MLO1,MHI1,0.0)
        CALL HBOOK1(15,'MASS LIKELIHOOD',XNMASS,MLO1,MHI1,0.0)
        CALL HBOOK1(21,'NUMBER OF EVENTS GENERATED',
     &    100,-0.5,99.5,0.0)
        CALL HBOOK1(31,'BRANCHING RATIO WW LIKELIHOOD',NBR,
     &    BRLO1,BRHI1,0.0)
C
        MLO1 = MLO-0.5*MSTEP
        MHI1 = MLO1 + NMASS*MSTEP
        CLO1 = CLO-0.5*CSTEP
        CHI1 = CLO1 + NCROSS*CSTEP
C
        CALL HBOOK2(1,'CROSS SECTION VS MASS NEV=17',
     &    NMASS,MLO1,MHI1, NCROSS,CLO1,CHI1,0.0)
        CALL HBOOK2(2,'CROSS SECTION VS MASS NEV.GT.17',
     &    NMASS,MLO1,MHI1, NCROSS,CLO1,CHI1,0.0)
C
      ENDIF
C
      DO IBR = 1 , NBR
        BRWW = BRLO + (IBR-1)*BRSTEP
        DO I = 1 , XNMASS
          IF ( FLUCTUATE_MASS ) THEN
            MASS = GAUSS(MASS_CENT,SIG_MASS,MLO)
            LIKE_MASS = 1.0
          ELSE
            MASS = XMLO + (I-1)*XMSTEP
            DEL = (MASS-MASS_CENT)/SIG_MASS
            LIKE_MASS = XMSTEP*EXP(-DEL**2)/(SQRT(TWOPI)*SIG_MASS)
C GAUSSIAN MASS LIKELIHOOD
          ENDIF
          IEN = 18
          XSECT = BRWW*CROSS_SECT(MASS)  !CENTRAL VALUE OF CROSS WSECTION
          ERRX = BRWW*XSECT*EXSECT
C
          DO K = 1 , NCH
            IF ( K.EQ.1 ) THEN
              SBR(K) = EMU_SIGBR(MASS)
              ESBR(K) = EMU_SERR(MASS)
            ELSEIF ( K.EQ.2 ) THEN
              SBR(K) = EE_SIGBR(MASS)
              ESBR(K) = EE_SERR(MASS)
            ELSEIF ( K.EQ.3 ) THEN
              SBR(K) = MUMU_SIGBR(MASS)
              ESBR(K) = MUMU_SERR(MASS)
            ELSEIF ( K.EQ.4 ) THEN
              SBR(K) = EJETS_SIGBR(MASS)
              ESBR(K) = EJETS_SERR(MASS)
            ELSEIF ( K.EQ.5 ) THEN
              SBR(K) = MUJETS_SIGBR(MASS)
              ESBR(K) = MUJETS_SERR(MASS)
            ELSEIF ( K.EQ.6 ) THEN
              SBR(K) = EJETS_TAG_SIGBR(MASS)
              ESBR(K) = EJETS_TAG_SERR(MASS)
            ELSEIF ( K.EQ.7 ) THEN
              SBR(K) = MUJETS_TAG_SIGBR(MASS)
              ESBR(K) = MUJETS_TAG_SERR(MASS)
            ENDIF
          ENDDO
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
                CALL ERRMSG(' DLIMIT',' CONSISTENCY ',
     &            ' ERROR IN POISSN ','W')
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
            IF ( FLUCTUATE_MASS ) THEN
              CALL DO_HF1(21,FLOAT(NTOTGEN),LIKE_MASS)
            ENDIF
          ENDDO
          PRB = FLOAT(NREP)/FLOAT(NEXPGEN)
          PRBG = FLOAT(NREPG)/FLOAT(NEXPGEN)
C
          CALL DO_HF1(11,MASS,PRB)
          CALL DO_HF1(12,MASS,PRBG)
          CALL DO_HF1(13,MASS,PRB*LIKE_MASS)
          CALL DO_HF1(14,MASS,PRBG*LIKE_MASS)
          CALL DO_HF1(15,MASS,LIKE_MASS)
          CALL DO_HF1(31,BRWW,PRB)
C
          WRITE(57,1)MASS,PRB,PRBG,LIKE_MASS
    1     FORMAT(1X,4F15.5)
        ENDDO
      ENDDO
C
      IF ( DO_CONTOUR ) THEN
C
C ****  now to do the cross section and mass plane
C
        DO I = 1 , NMASS
          MASS = MLO + (I-1)*MSTEP
          DEL = (MASS-MASS_CENT)/SIG_MASS
          LIKE_MASS = MSTEP*EXP(-DEL**2)/(SQRT(TWOPI)*SIG_MASS)
C GAUSSIAN MASS LIKELIHOOD
          DO K = 1 , NCH
            IF ( K.EQ.1 ) THEN
              SBR(K) = EMU_SIGBR(MASS)
              ESBR(K) = EMU_SERR(MASS)
            ELSEIF ( K.EQ.2 ) THEN
              SBR(K) = EE_SIGBR(MASS)
              ESBR(K) = EE_SERR(MASS)
            ELSEIF ( K.EQ.3 ) THEN
              SBR(K) = MUMU_SIGBR(MASS)
              ESBR(K) = MUMU_SERR(MASS)
            ELSEIF ( K.EQ.4 ) THEN
              SBR(K) = EJETS_SIGBR(MASS)
              ESBR(K) = EJETS_SERR(MASS)
            ELSEIF ( K.EQ.5 ) THEN
              SBR(K) = MUJETS_SIGBR(MASS)
              ESBR(K) = MUJETS_SERR(MASS)
            ELSEIF ( K.EQ.6 ) THEN
              SBR(K) = EJETS_TAG_SIGBR(MASS)
              ESBR(K) = EJETS_TAG_SERR(MASS)
            ELSEIF ( K.EQ.7 ) THEN
              SBR(K) = MUJETS_TAG_SIGBR(MASS)
              ESBR(K) = MUJETS_TAG_SERR(MASS)
            ENDIF
          ENDDO
          DO IC = 1 , NCROSS
            CROSS = CLO + (IC-1)*CSTEP
            NREP = 0
            NREPG = 0
            DO J = 1 , NEXPGEN
C
C ****  GENERATE THESE MANY EXPERIMENTS AT THIS MASS AND ASK HOW OFTEN
C ****  TOTAL NUMBER OF EVENTS OBSERVED = ACTUAL NUMBER.
C ****  HISTOGRAM MASS WITH THIS PROBABILITY AS WEIGHT.
C ****  FIND MAXIMUM LIKELIHOOD MASS
C
              NTOTGEN = 0 !TOTAL EVENTS GENERATED
              DO K = 1 , NCH
                ALUM = GAUSS(LUMIN(K),ELUMIN(K),0.0)
                SB   = GAUSS(SBR(K),ESBR(K),0.0)
                EVEXP(K) = ALUM*CROSS*SB
                CALL POISSN(EVEXP(K),NGENEXP(K),IER)
                IF ( IER.NE.0 ) THEN
                  CALL ERRMSG(' DLIMIT',' CONSISTENCY ',
     &              ' ERROR IN POISSN ','W')
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
            ENDDO
            PRB = FLOAT(NREP)/FLOAT(NEXPGEN)
            PRBG = FLOAT(NREPG)/FLOAT(NEXPGEN)
            NLIKE = (PRB*LIKE_MASS)
            IF ( NLIKE.GT.0.0 ) THEN
              NLIKE = -LOG(NLIKE)
            ELSE
              NLIKE = 0.0  !TRY THIS
            ENDIF
            CALL DO_HF2(1,MASS,CROSS,NLIKE)
            NLIKE = (PRBG*LIKE_MASS)
            IF ( NLIKE.GT.0.0 ) THEN
              NLIKE = -LOG(NLIKE)
            ELSE
              NLIKE = 0.0  !TRY THIS
            ENDIF
            CALL DO_HF2(2,MASS,CROSS,NLIKE)
          ENDDO
        ENDDO
      ENDIF
  999 RETURN
      END
