      SUBROUTINE MODULE_MARKERS( LCLGA, IDENT, X, Y, Z, NC, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURN AN ARRAY OF THEORETICAL POSITIONS
C-                         OF THE SURVEY MARKERS FOR A PARTICULAR
C-                         MODULE ORIENTED IN SPACE.
C-
C-   Inputs  :        LCLGA      pointer to FH part of module (upstream)
C-                    IDENT      flag to indicate south (pos) or north(neg)
C-   Outputs :        X          array of X positions of markers
C-                    Y          array of Y positions of markers
C-                    Z          array of Z positions of markers
C-                    NC         number of markers for this module
C-                    IERR       error flag
C-   Controls: 
C-
C-   Created  16-APR-1992   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
C
      REAL    ARRAY(40), PSI, SGN
      REAL    X(*), Y(*), Z(*)
      INTEGER LCLGA, IDENT, IERR, NC, MOD_TYPE, MOD_NUMB, J 
      INTEGER IERR1, IERR2, IERR3
      LOGICAL FIRST
      CHARACTER ARRAY_NAME*30, ECNS*4
      REAL    DL, FIXPT(3)
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C     IF (FIRST) THEN
C       CALL INRCP('CAL_SURVEY_MARKERS_RCP', IERR)
C       IF( IERR .NE. 0) THEN
C         CALL ERRMSG('NO CAL_SURVEY_MARKERS_RCP','MODULE_MARKERS',
C    +      'can not find EC theo positions--stop','S')
C         STOP 610
C       END IF
C       FIRST = .FALSE.
C     END IF
C
      CALL EZPICK('CAL_SURVEY_MARKERS_RCP')
      SGN = SIGN(1.,FLOAT(IDENT))
      CALL EZGET('EC_THERMAL_COEFF',DL,IERR)      ! RCP numbers are warm
      CALL EZGET('EC_FIXED_POINT', FIXPT, IERR1)
      IF( IERR .NE. 0 .OR. IERR1 .NE.0) THEN
        CALL ERRMSG('NO_THERMAL_CONTRACTION','MODULE_MARKERS',
     +    'thermal contraction params not found','W')
        DL = 1.0
        CALL VZERO(FIXPT, 3)
      END IF
      FIXPT(3) = FIXPT(3)*SGN
C
C
      MOD_TYPE = 100*(IC(LCLGA+IGIDEN)/100)       ! module id
      MOD_NUMB = MOD(IC(LCLGA+IGIDEN),100)        ! module number
C
      IF( MOD_TYPE .EQ. ICMFHA )THEN              ! MH module
        CALL EZGET('MH_DOWNSTREAM_INNER_MARKER',ARRAY,IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('NO MH_DOWNSTREAM_INNER','MODULE_MARKERS',
     +      'can not find EC theo positions--stop','S')
          STOP 611
        END IF
        CALL EZGET('MH_DOWNSTREAM_OUTER_MARKER',ARRAY(4),IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('NO MH_D0WNSTREAM_OUTER','MODULE_MARKERS',
     +      'can not find EC theo positions--stop','S')
          STOP 612
        END IF
        CALL EZGET('MH_UPSTREAM_INNER_MARKER',ARRAY(7),IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('NO MH_UPSTREAM_INNER','MODULE_MARKERS',
     +      'can not find EC theo positions--stop','S')
          STOP 613
        END IF
        CALL EZGET('MH_UPSTREAM_OUTER_MARKER',ARRAY(10),IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('NO MH_UPSTREAM_OUTER','MODULE_MARKERS',
     +      'can not find EC theo positions--stop','S')
          STOP 614
        END IF
        X(1) = -ARRAY(2)
        Y(1) = ARRAY(1)
        Z(1) = SGN*ARRAY(3)
        X(2) = -ARRAY(11)       ! only downstream_inner and 
        Y(2) = ARRAY(10)       ! upstream outer are used in survey
        Z(2) = SGN*ARRAY(12)
        CALL COLD_SHRINK(X(1), Y(1), Z(1), DL, FIXPT)  ! do thermal contraction
        CALL COLD_SHRINK(X(2), Y(2), Z(2), DL, FIXPT)
        DO 100 J = 2, 16
          PSI = C(LQCLGA+IGDPHI)*(J-1)
          X(2*J-1) = X(1)*COS(PSI) - Y(1)*SIN(PSI)
          Y(2*J-1) = X(1)*SIN(PSI) + Y(1)*COS(PSI)
          Z(2*J-1) = Z(1)
          X(2*J) = X(2)*COS(PSI) - Y(2)*SIN(PSI)
          Y(2*J) = X(2)*SIN(PSI) + Y(2)*COS(PSI)
          Z(2*J) = Z(2)
  100   CONTINUE
        NC = 32
      ELSE IF( MOD_TYPE .EQ. ICOCHA) THEN         ! OH module
          CALL EZGET('OH_DOWNSTREAM_INNER_MARKER', ARRAY, IERR)
          IF( IERR .NE. 0) THEN
              CALL ERRMSG('NO OH_DOWNSTREAM_INNER','MODULE_MARKERS',
     +          'can not find EC theo positions--stop','S')
              STOP 621
          END IF
          CALL EZGET('OH_DOWNSTREAM_OUTER_MARKER', ARRAY(4), IERR)
          IF( IERR .NE. 0) THEN
              CALL ERRMSG('NO OH_DOWNSTREAM_OUTER','MODULE_MARKERS',
     +          'can not find EC theo positions--stop','S')
              STOP 622
          END IF
          CALL EZGET('OH_UPSTREAM_INNER_MARKER', ARRAY(7), IERR)
          IF( IERR .NE. 0) THEN
              CALL ERRMSG('NO OH_UPSTREAM_INNER','MODULE_MARKERS',
     +          'can not find EC theo positions--stop','S')
              STOP 623
          END IF
          CALL EZGET('OH_UPSTREAM_OUTER_MARKER', ARRAY(10), IERR)
          IF( IERR .NE. 0) THEN
              CALL ERRMSG('NO OH_UPSTREAM_OUTER','MODULE_MARKERS',
     +          'can not find EC theo positions--stop','S')
              STOP 624
          END IF  

          X(1) = -ARRAY(2)
          Y(1) = ARRAY(1)
          Z(1) = SGN*ARRAY(3)
          X(2) = -ARRAY(5)
          Y(2) = ARRAY(4)
          Z(2) = SGN*ARRAY(6)
          X(3) = -ARRAY(8)
          Y(3) = ARRAY(7)
          Z(3) = SGN*ARRAY(9)
          X(4) = -ARRAY(11)
          Y(4) = ARRAY(10)
          Z(4) = SGN*ARRAY(12)
          CALL COLD_SHRINK(X(1), Y(1), Z(1), DL, FIXPT)  ! do thermal contract
          CALL COLD_SHRINK(X(2), Y(2), Z(2), DL, FIXPT)
          CALL COLD_SHRINK(X(3), Y(3), Z(3), DL, FIXPT)  
          CALL COLD_SHRINK(X(4), Y(4), Z(4), DL, FIXPT)

          DO 200 J = 2, 16
          PSI = C(LQCLGA + IGDPHI)*(J-1)
          X(4*J-3) = X(1)*COS(PSI) - Y(1)*SIN(PSI)
          Y(4*J-3) = X(1)*SIN(PSI) + Y(1)*COS(PSI)
          Z(4*J-3) = Z(1)
          X(4*J-2) = X(2)*COS(PSI) - Y(2)*SIN(PSI)
          Y(4*J-2) = X(2)*SIN(PSI) + Y(2)*COS(PSI)
          Z(4*J-2) = Z(2)
          X(4*J-1) = X(3)*COS(PSI) - Y(3)*SIN(PSI)
          Y(4*J-1) = X(3)*SIN(PSI) + Y(3)*COS(PSI)
          Z(4*J-1) = Z(3)
          X(4*J) = X(4)*COS(PSI) - Y(4)*SIN(PSI)
          Y(4*J) = X(4)*SIN(PSI) + Y(4)*COS(PSI)
          Z(4*J) = Z(4)
  200     CONTINUE
          NC = 64
      ELSE IF( MOD_TYPE .EQ. ICIFHA) THEN         ! IH module
        IF(IDENT .LT. 0) THEN
          ECNS = 'ECN_'
        ELSE
          ECNS = 'ECS_'
        END IF
        ARRAY_NAME = ECNS//'IH_UPSTREAM_BOT'
        CALL EZGET(ARRAY_NAME, ARRAY(7), IERR)
        ARRAY_NAME = ECNS//'IH_UPSTREAM_WEST'
        CALL EZGET(ARRAY_NAME, ARRAY(4), IERR1)
        ARRAY_NAME = ECNS//'IH_UPSTREAM_TOP'
        CALL EZGET(ARRAY_NAME ,ARRAY(1), IERR2)
        ARRAY_NAME = ECNS//'IH_UPSTREAM_EAST'
        CALL EZGET(ARRAY_NAME ,ARRAY(10), IERR3)
        IF( IERR+IERR1+IERR2+IERR3 .NE. 0) THEN
          CALL ERRMSG('NO_IH_UPSTREAM','MODULE_MARKERS',
     +      'can not find IH upstream theo positions','S')
          STOP 631
        END IF
        ARRAY_NAME = ECNS//'IH_DOWNSTREAM_BOT'
        CALL EZGET(ARRAY_NAME, ARRAY(19), IERR)
        ARRAY_NAME = ECNS//'IH_DOWNSTREAM_WEST'
        CALL EZGET(ARRAY_NAME, ARRAY(16), IERR1)
        ARRAY_NAME = ECNS//'IH_DOWNSTREAM_TOP'
        CALL EZGET(ARRAY_NAME, ARRAY(13), IERR2)
        ARRAY_NAME = ECNS//'IH_DOWNSTREAM_EAST'
        CALL EZGET(ARRAY_NAME, ARRAY(22), IERR3)
        IF( IERR+IERR1+IERR2+IERR3 .NE. 0) THEN
          CALL ERRMSG('NO_IH_DOWNSTREAM','MODULE_MARKERS',
     +      'can not find IH downstream theo positions','S')
          STOP 632
        END IF
        DO 300 J = 1, 4
        X(J+4) = ARRAY( 3*(J-1)+1)
        Y(J+4) = ARRAY( 3*(J-1)+2)
        Z(J+4) = ARRAY( 3*(J-1)+3)
        X(J) = ARRAY( 3*(J-1)+13)
        Y(J) = ARRAY( 3*(J-1)+14)
        Z(J) = ARRAY( 3*(J-1)+15)
        CALL COLD_SHRINK(X(J), Y(J), Z(J), DL, FIXPT)  ! do thermal contraction
        CALL COLD_SHRINK(X(J+4), Y(J+4), Z(J+4), DL, FIXPT)
  300   CONTINUE
        NC = 8
      ELSE IF( MOD_TYPE .EQ. ICEEMA) THEN         ! EC/EM module
        CALL EZGET('ECEM_SUPPORT_BOT', ARRAY(7), IERR)
        CALL EZGET('ECEM_SUPPORT_WEST', ARRAY(4), IERR1)
        CALL EZGET('ECEM_SUPPORT_TOP', ARRAY(1), IERR2)
        CALL EZGET('ECEM_SUPPORT_EAST', ARRAY(10), IERR3)
        IF( IERR+IERR1+IERR2+IERR3 .NE. 0) THEN
          CALL ERRMSG('NO_EC/EM_SUPP_PLT','MODULE_MARKERS',
     +      'can not find EC/EM theo positions','S')
          STOP 641
        END IF
        CALL EZGET('ECEM_CENTER_PLUG', ARRAY(13), IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('NO_EC/EM_CNTR_PLG','MODULE_MARKERS',
     +      'can not find EC/EM theo positions','S')
          STOP 642
        END IF
        DO 400 J = 1, 5
        X(J) = ARRAY( 3*(J-1)+1)
        Y(J) = ARRAY( 3*(J-1)+2)
        Z(J) = SGN*ARRAY( 3*(J-1)+3)
        CALL COLD_SHRINK(X(J), Y(J), Z(J), DL, FIXPT)  ! do thermal contraction
  400   CONTINUE
        NC = 5
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
