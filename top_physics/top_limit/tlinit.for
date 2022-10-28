      SUBROUTINE TLINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize top limit program
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-JUL-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      LOGICAL OK
C
      INTEGER I
      INTEGER IER
      INTEGER ID
      INTEGER LEN
      INTEGER NENTRIES
      INTEGER CH_SW(100)
C
      REAL ERROR
C
      REAL*8 DGAMMA
C
      CHARACTER*74 FILE
C
C----------------------------------------------------------------------
C
C  Initialize zebra
C
      CALL MZEBRA(0)
      CALL INZSTP
C
C  Read in RCP file and pick RCP bank
C
c      LMSG = 6
      CALL INRCP('TOP_LIMIT_RCP',IER)
      IF (IER.NE.0) GOTO 900
      CALL EZPICK('TOP_LIMIT_RCP')
C
C  Open the message output file
C
      CALL EZGET_i('TOP_LIMIT_ID',ID,IER)
      IF (IER.EQ.0) CALL EZGETS('MSG_FILE',1,FILE,LEN,IER)
      IF (IER.NE.0) GOTO 900
      CALL GTUNIT(ID,LMSG,IER)
      IF (IER.NE.0) GOTO 910
      CALL D0OPEN(LMSG,FILE,'OF',OK)
      IF (.NOT.OK) GOTO 910
C
C  Read parameters
C
      CALL EZGETA('OBSERVED_SIGNAL',0,0,0,NENTRIES,IER)
      IF (IER.EQ.0) CALL EZGET_iarr('CHANNEL_SWITCH',CH_SW,IER)
      IF (IER.EQ.0) CALL EZGET_iarr('OBSERVED_SIGNAL',N,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('EXPECTED_BG',BG,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('BG_ERROR',DBG,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('LUMINOSITY',LUM,IER)
      IF (IER.EQ.0) CALL EZGET('LUMINOSITY_ERROR',DLUM,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('BRANCHING_RATIO',BR,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('EFFICIENCY',EFF,IER)
      IF (IER.EQ.0) CALL EZGET_rarr('EFFICIENCY_ERROR',DEFF,IER)
      IF (IER.EQ.0) CALL EZ_GET_CHARS('CHANNEL_NAMES',NCHAN,NAMES,IER)
      IF (IER.EQ.0) CALL EZGET_i('TOP_MASS',TOP_MASS,IER)
      IF (IER.EQ.0) CALL EZGET('CONFIDENCE_LEVEL',CL,IER)
      IF (IER.EQ.0) CALL EZGET('ERROR',ERROR,IER)
      IF (IER.EQ.0) CALL EZGET_d('GAMMA',GAMMA,IER)
      IF (IER.EQ.0) CALL EZGET_i('NGEN',NGEN,IER)
      IF (IER.NE.0) GOTO 900
      EPS = ERROR
      CALL EZRSET
C
C  Turn channels on if CHANNEL_SWITCH value set to 1, off=0.
C
      NCHAN=0
      DO I=1,NENTRIES
        IF ( CH_SW(I).EQ.1 ) THEN
          NCHAN=NCHAN+1
          N(NCHAN)     =N(I)
          BG(NCHAN)    =BG(I)
          DBG(NCHAN)   =DBG(I)
          LUM(NCHAN)   =LUM(I)
          BR(NCHAN)    =BR(I)
          EFF(NCHAN)   =EFF(I)
          DEFF(NCHAN)  =DEFF(I)
          NAMES(NCHAN) =NAMES(I)
        ENDIF
      ENDDO
C
C  Print out input values
C
      WRITE (LMSG,1000) TOP_MASS
      DO I=1,NCHAN
        WRITE (LMSG,1001) NAMES(I),N(I),BG(I),DBG(I),LUM(I),BR(I),
     &    EFF(I),DEFF(I)
      ENDDO
      WRITE (LMSG,1002) DLUM,100.*CL,NGEN,ERROR,GAMMA
C
C  Relate top cross section to expected number of events
C
C        N_EXP = BETA + SIGMA*ALPHA
C
C  where BETA is the expected background and ALPHA is given below
C
      NTOT = 0
      ALPHA0 = 0.
      DALPHA = 0.
      BETA0 = 0.
      DBETA = 0.
      DO I=1,NCHAN
        NTOT = NTOT + N(I)
        ALPHA0 = ALPHA0 + LUM(I)*EFF(I)*BR(I)
        DALPHA = DALPHA + (LUM(I)*BR(I)*DEFF(I))**2
        BETA0 = BETA0 + BG(I)
        DBETA = DBETA + DBG(I)**2
      ENDDO
      DALPHA = DSQRT(DALPHA + (DLUM*ALPHA0/100.)**2)
      DBETA = DSQRT(DBETA)
      FACTORIAL = DGAMMA(DFLOAT(NTOT+1))
C
C Calculate integration range for the first integration over ALPHA
C
      ALPHA_MIN = ALPHA0 - GAMMA*DALPHA
      IF (ALPHA_MIN.LT.0.0D0) ALPHA_MIN = 0.0D0
      ALPHA_MAX = ALPHA0 + GAMMA*DALPHA
C
C Calculate integration range for the second integration over BETA
C
      BETA_MIN = BETA0 - GAMMA*DBETA
      IF (BETA_MIN.LT.0.0D0) BETA_MIN = 0.0D0
      BETA_MAX = BETA0 + GAMMA*DBETA
C
C  Calculate the error in sigma (assuming Gaussian statistics)
C
      DSIGMA = DSQRT(AMAX0(NTOT,1) + DBETA**2 +
     &  (NTOT-BETA0)**2*DALPHA**2/ALPHA0**2)/ALPHA0
      GOTO 999
C
  900 WRITE (LMSG,1100)
      GOTO 920
  910 WRITE (LMSG,1101)
  920 STOP
C
  999 RETURN
C
 1000 FORMAT(//' TOP CROSS SECTION LIMIT CALCULATION FOR',I4,
     &  ' GEV TOP MASS')
 1001 FORMAT(//' PARAMETERS FOR THE CHANNEL ',A10//
     &  4X,'Signal Events:',T25,I10/
     &  4X,'Expected Background:',T25,F10.2/
     &  4X,'Background Error:',T25,F10.2/
     &  4X,'Luminosity:',T25,F10.2/
     &  4X,'Branching Ratio:',T25,F10.5/
     &  4X,'Efficiency:',T25,F10.4/
     &  4X,'Efficiency Error:',T25,F10.4)
 1002 FORMAT(//' GENERAL PARAMETERS'//
     &  4X,'Luminosity Error:',T25,F10.1,'%'/
     &  4X,'Confidence Level:',T25,F10.1,'%'/
     &  4X,'MC Simulations:',T25,I10/
     &  4x,'Numerical Accuracy:',T25,1PE10.1/
     &  4X,'Cutoff on Gaussians:',T25,0PF10.1,' s.d.')
 1100 FORMAT(' RCP error - Quiting!')
 1101 FORMAT(' Error opening message file - Quiting!')
C
      END
