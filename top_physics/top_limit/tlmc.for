      SUBROUTINE TLMC(SIG_CL,DERIV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test top limit method via MC method
C-
C-   Inputs  : SIG_CL - Top cross section
C-             DERIV  - Derivative of CL wrt cross section
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUN-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      REAL*8 SIG_CL
      REAL*8 DERIV
C
      INTEGER I,II
      INTEGER NMC(10)
      INTEGER NPROD
      INTEGER NGOOD
      INTEGER IER
C
      REAL RN
      REAL BGMC
      REAL LUMMC,LUMFAC
      REAL EFFMC
C
      REAL*8 BGSUM
      REAL*8 TERM
      REAL*8 PROB_BG
      REAL*8 WGT_FAIL
      REAL*8 RATIO,DRATIO
      REAL*8 RATIOC,DRATIOC
      REAL*8 SIG_MC,DSIG_MC
C
C----------------------------------------------------------------------
C
C  Generate a new experiment
C
      NGOOD = 0
      WGT_FAIL = 0.
      DO II=1,NGEN
  100   CALL NORRAN(RN)
        LUMFAC = 1 + DLUM*RN/100.
        IF (LUMFAC.LT.0.) GOTO 100
        BGSUM = 0.
        DO I=1,NCHAN
  200     CALL NORRAN(RN)
          BGMC = BG(I) + DBG(I)*RN
          IF (BGMC.LT.0.) GOTO 200
          BGSUM = BGSUM + BGMC
  300     CALL NORRAN(RN)
          EFFMC = EFF(I) + DEFF(I)*RN
          IF ((EFFMC.LT.0.).OR.(EFFMC.GT.1.)) GOTO 300
          LUMMC = LUMFAC * LUM(I)
          CALL POISSN(SIG_CL*BR(I)*LUMMC*EFFMC+BGMC,NMC(I),IER)
        ENDDO
C
C  Calculate total number of signal and background events
C
        NPROD = 0
        DO I=1,NCHAN
          NPROD = NPROD + NMC(I)
        ENDDO
        IF (NPROD.GT.NTOT) NGOOD = NGOOD + 1
C
C  Calculate correction factor for probability BG has > NTOT events
C
        I = 0
        PROB_BG = 0.
        TERM =  DEXP(-BGSUM)
        DO WHILE (I.LE.NTOT)
          PROB_BG = PROB_BG + TERM
          I = I + 1
          TERM = TERM*BGSUM/DFLOAT(I)
        ENDDO
          IF (NPROD.LE.NTOT) WGT_FAIL = WGT_FAIL + 1./PROB_BG
      ENDDO
C
C  Calculate results
C
      RATIO = FLOAT(NGOOD)/FLOAT(NGEN)
      DRATIO = SQRT(NGOOD*(1.-RATIO))/FLOAT(NGEN)
      RATIOC = 1 - WGT_FAIL/FLOAT(NGEN)
      DRATIOC = DRATIO*WGT_FAIL/FLOAT(MAX(NGEN-NGOOD,1))
      SIG_MC = (CL - RATIOC)/DERIV + SIG_CL
      DSIG_MC = DRATIOC/DERIV
C
C  Write out results
C
      WRITE (LMSG,1000) SIG_CL,NTOT,NGOOD,NGEN,NTOT,100.*RATIO,
     &  100.*DRATIO,100.*RATIOC,100.*DRATIOC,100.*CL,SIG_MC,DSIG_MC
C
  999 RETURN
C
 1000 FORMAT(//' MC SIMULTATION OF TOP CROSS SECTION LIMIT'//
     &  4X,'Cross Section for Simulation:',T45,F10.3,' pb'/
     &  4X,'Experiments with more than',I3,' Events:',T45,I10/
     &  4X,'Total Number of Experiments:',T45,I10/
     &  4X,'Fraction with more than',I3,' Events:',T45,F10.3,
     &  '% +-',F6.3,'%'/
     &  4x,'Fraction with BG Renormalization:',T45,F10.3,
     &  '% +-',F6.3,'%'/
     &  4X,'MC Cross Section at',F5.1,'% CL:',T45,F10.3,
     &  '  +-',F6.3,' pb')
C
      END
