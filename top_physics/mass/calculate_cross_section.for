      SUBROUTINE CALCULATE_CROSS_SECTION
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the born term for the top cross section
C-   for all top masses under consideration.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
      DOUBLE PRECISION XA_MIN,XA_MAX,DGMLT3,FSUB3
      DOUBLE PRECISION A3,B3,X(3)
      INTEGER NI3,NG3
      LOGICAL USE_FITTED
      EXTERNAL FSUB3
      REAL    FITTED_CROSS
      DOUBLE PRECISION  TMASSEE
      LOGICAL READ_CROSS
      INTEGER TCUNI ,TCUNO
      CHARACTER*80 TOP_CROSS_SECTION_INPUT
      CHARACTER*80 TOP_CROSS_SECTION_OUTPUT
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('ROOTS_TEV',ROOTS_TEV,IER)
        CALL EZGET('QCD_LAMBDA',ALAM,IER)
        CALL EZGET('ALPHA_STRONG',ALPHA_S,IER)
        CALL EZGET('CONV_PB',CONV_PB,IER)
        CALL EZGET('NGAUSS',NG3,IER)
        CALL EZGET('NSUB_DIV',NI3,IER)
        CALL EZGET('USE_FITTED_CROSS_SECTION',USE_FITTED,IER)
        CALL EZGET('READ_CROSS_SECTION',READ_CROSS,IER)
        IF ( READ_CROSS ) THEN
          CALL EZ_FILE_OPEN(1,'TOP_CROSS_SECTION_INPUT','IU',TCUNI,
     &      TOP_CROSS_SECTION_INPUT,IER)
        ELSE
          CALL EZ_FILE_OPEN(1,'TOP_CROSS_SECTION_OUTPUT','OU',TCUNO,
     &      TOP_CROSS_SECTION_OUTPUT,IER)
        ENDIF
        ALAM2 = ALAM*ALAM
        CALL EZRSET
C
        TMASSE = TMASS_LO
        NTOPS = 0
        DO WHILE (TMASSE.GE.TMASS_LO.AND.TMASSE.LE.TMASS_HI)
          NTOPS = NTOPS + 1
          IF ( NTOPS.GT.MAXT ) THEN
            CALL ERRMSG('TOP_DILEPTON','ANALYZE_DILEPTON_EVENT',
     &        'TOO MANY TOP SOLUTIONS REQUESTED ','W')
            NTOPS = MAXT
          ENDIF
          TOP_MASS(NTOPS)=TMASSE
          TAU_MIN = 4.0*(TMASSE/ROOTS_TEV)**2
          A3 = TAU_MIN
          B3= 1.0D0
          IF ( USE_FITTED ) THEN
            TOP_QQB(NTOPS) = FITTED_CROSS('QUARK',TMASSE)
            TOP_GG(NTOPS) = FITTED_CROSS('GLUON',TMASSE)
            TOP_CROSS(NTOPS) = FITTED_CROSS('TOTAL',TMASSE)
          ELSEIF ( READ_CROSS ) THEN
C READ CROSS SECTION FROM FILE
            READ(TCUNI)TMASSEE,TOP_CROSS(NTOPS),TOP_GG(NTOPS),
     &        TOP_QQB(NTOPS)
            IF ( TMASSEE.NE.TMASSE ) THEN
              CALL ERRMSG('TOP_MASS','CALCULATE_CROSS_SECTION',
     &          ' TOP MASS IN FILE IS NOT CURRENT MASS','W')
            ENDIF
          ELSE
            QUARK=.TRUE.
            TOP_QQB(NTOPS) = DGMLT3(FSUB3,A3,B3,NI3,NG3,X)
            QUARK=.FALSE.
            TOP_GG(NTOPS) = DGMLT3(FSUB3,A3,B3,NI3,NG3,X)
            TOP_CROSS(NTOPS) = TOP_QQB(NTOPS) + TOP_GG(NTOPS)
          ENDIF
C
          CALL HFILL(3001,TOP_MASS(NTOPS),0.,TOP_CROSS(NTOPS))
          CALL HFILL(3002,TOP_MASS(NTOPS),0.,TOP_QQB(NTOPS))
          CALL HFILL(3003,TOP_MASS(NTOPS),0.,TOP_GG(NTOPS))
C
          IF ( .NOT.READ_CROSS ) THEN

            WRITE(TCUNO)TMASSE,TOP_CROSS(NTOPS),TOP_GG(NTOPS),
     &        TOP_QQB(NTOPS)
          ENDIF
C
          TMASSE = TMASSE + DELMASS
        ENDDO
        CLOSE(UNIT=TCUNI)
        CLOSE(UNIT=TCUNO)
C
      ENDIF
  999 RETURN
      END
