      SUBROUTINE L2JETS_JPARFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill JPAR bank with the parameters used for
C               this runs L2JETS tool.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JUL-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'
      INCLUDE 'D0$INC:L2JETS_PAR.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LJPAR,I,GZSL2H,LSUP,NREP
C----------------------------------------------------------------------

      CALL BKJPAR(NUM_PARAMS,LJPAR)
      IF (LJPAR .LE. 0) THEN
        CALL ERRMSG('BOOK FAILURE','L2JETS_JPARFL','Cannot book JPAR',
     &    'E')
        NOWERR = ERR_UTILITY
        RETURN
      END IF
C
      IC(LJPAR + 1) = 1
      IC(LJPAR + 2) = NPAR_VAL_MAX       ! # of parameter values
      IC(LJPAR + 3) = NUM_PARAMS         ! # of parameter sets
      NREP = NPAR_VAL_MAX + 1
      DO I = 1,NUM_PARAMS
        C(LJPAR + 4 + (I-1)*NREP) = FLOAT(NJET_COUNT(I))
        C(LJPAR + 5 + (I-1)*NREP) = ETMIN(I)
        C(LJPAR + 6 + (I-1)*NREP) = FLOAT(ICON_CEN(I))*.2 + .1
        C(LJPAR + 7 + (I-1)*NREP) = FLOAT(ICON_ENG(I))*.2 + .1
        C(LJPAR + 8 + (I-1)*NREP) = MAXRAD(I)
        C(LJPAR + 9 + (I-1)*NREP) = MINRAD(I)
        C(LJPAR + 10 + (I-1)*NREP) = EMFRACT_MAX(I)
        C(LJPAR + 11 + (I-1)*NREP) = EMFRACT_MIN(I)
        C(LJPAR + 12 + (I-1)*NREP) = FLOAT( IND_PARAM( I ) )
        IF ( IC( LJPAR + 1 ) .GT. 1 ) THEN
          C( LJPAR + 13 + (I-1)*NREP ) = 0.
          IF ( VETO_THIS(I) ) C( LJPAR + 13 + (I-1)*NREP ) = 1.
        ENDIF
      END DO
  999 RETURN
      END
